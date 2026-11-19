# Declarative feature pipelines ----------------------------------------------
#
# A feature pipeline is a set of `feature_node()`s. Each node declares the
# columns it consumes (`inputs`), the columns it produces (`outputs`), and the
# nodes it builds upon (`depends`). Nodes run in dependency order, each one
# exactly once per table, and each node only ever sees the columns it declared.
#
# Every node result is matched back on the key/index of the table it was
# computed from. A node may reorder its rows freely, but it can never borrow a
# value from another series or another time point: dropping, duplicating or
# inventing rows is an error, not a silent shift.

#' Declarative feature nodes
#'
#' @description
#' A feature node is a single, self-contained step of a [feature_pipeline()].
#' It declares
#'
#' * `inputs`: the columns it is allowed to read,
#' * `outputs`: the columns it produces,
#' * `depends`: the nodes that must run before it.
#'
#' The node function `fn` receives a tsibble that holds the key and index
#' columns plus **only** the declared `inputs`, grouped by the key so that
#' time-series operations such as [dplyr::lag()] stay within each series. It
#' must return a data frame holding the key, the index and its `outputs`. The
#' returned rows may be in any order: they are matched back on key and index.
#'
#' @param name A single string identifying the node. Other nodes refer to it in
#' their `depends`.
#' @param fn A function (or purrr-style lambda) that computes the node. It takes
#' a single tsibble and returns a data frame.
#' @param outputs A character vector of the column names `fn` produces.
#' @param inputs A character vector of the column names `fn` is allowed to read.
#' @param depends A character vector of node names that must be computed first.
#' Dependencies implied by an `input` matching another node's `outputs` are
#' added automatically.
#' @param stage One of `"both"` (default), `"history"` or `"future"`, indicating
#' the table(s) the node applies to. Only `"both"` and `"history"` nodes run on
#' the historical table; only `"both"` and `"future"` nodes run on the future
#' table.
#'
#' @return A feature node specification.
#' @seealso [feature_pipeline()], [compute_features()], [feature_forecast()]
#' @examples
#' # A lagged input, computed per series
#' lag_x <- feature_node(
#'   "lag_x", function(x) dplyr::mutate(x, x_lag = dplyr::lag(x)),
#'   outputs = "x_lag", inputs = "x"
#' )
#' lag_x
#' @export
feature_node <- function(name, fn, outputs, inputs = character(),
                         depends = character(),
                         stage = c("both", "history", "future")) {
  stage <- match.arg(stage)
  if (!is_string(name) || !nzchar(name)) {
    abort("`name` must be a single non-empty string.")
  }
  if (missing(fn)) {
    abort(sprintf("Feature node `%s` must supply a function in `fn`.", name))
  }
  if (missing(outputs)) {
    abort(c(
      sprintf("Feature node `%s` must declare the columns it produces.", name),
      i = "Supply a character vector to `outputs`."
    ))
  }
  if (is.character(outputs) && anyDuplicated(outputs)) {
    dup <- unique(outputs[duplicated(outputs)])
    abort(sprintf(
      "Feature node `%s` declares duplicated output(s) %s.",
      name, comma(backticks(dup))
    ))
  }
  inputs <- feature_chr(inputs, "inputs", name)
  outputs <- feature_chr(outputs, "outputs", name)
  depends <- feature_chr(depends, "depends", name)
  if (is_empty(outputs)) {
    abort(sprintf("Feature node `%s` must produce at least one column.", name))
  }
  if (anyDuplicated(outputs)) {
    dup <- unique(outputs[duplicated(outputs)])
    abort(sprintf(
      "Feature node `%s` declares duplicated output(s) %s.",
      name, comma(backticks(dup))
    ))
  }
  both <- intersect(inputs, outputs)
  if (!is_empty(both)) {
    abort(sprintf(
      "Feature node `%s` can't both consume and produce %s.",
      name, comma(backticks(both))
    ))
  }
  structure(
    list(
      name = name, fn = as_function(fn), inputs = inputs,
      outputs = outputs, depends = depends, stage = stage
    ),
    class = "tsbl_feature_node"
  )
}

feature_chr <- function(x, arg, node) {
  if (is_null(x)) {
    return(character())
  }
  if (is.character(x) && !anyNA(x) && all(nzchar(x))) {
    return(vec_unique(x))
  }
  abort(sprintf(
    "`%s` of feature node `%s` must be a character vector of column names.",
    arg, node
  ))
}

#' @export
print.tsbl_feature_node <- function(x, ...) {
  cat_line("<feature node> ", x$name, " (", x$stage, ")")
  cat_line(" inputs:  ", feature_chr_print(x$inputs))
  cat_line(" outputs: ", feature_chr_print(x$outputs))
  cat_line(" depends: ", feature_chr_print(x$depends))
  invisible(x)
}

feature_chr_print <- function(x) {
  if (is_empty(x)) "(none)" else comma(backticks(x))
}

#' Declarative feature pipelines
#'
#' @description
#' `feature_pipeline()` bundles [feature_node()]s into a directed acyclic graph.
#' The pipeline is only a specification: no data is touched until
#' [compute_features()] or [feature_forecast()] is called.
#'
#' Building a pipeline validates the graph. Duplicated node names, duplicated
#' output columns, unknown dependencies and dependency cycles all fail
#' immediately.
#'
#' @param ... `feature_node()` specifications, or a single list of them.
#'
#' @return A feature pipeline specification.
#' @seealso [feature_node()], [compute_features()], [feature_forecast()]
#' @examples
#' x1 <- feature_node(
#'   "x1", function(x) dplyr::mutate(x, x1 = dplyr::lag(x)),
#'   outputs = "x1_lag", inputs = "x"
#' )
#' x2 <- feature_node(
#'   "x2", function(x) dplyr::mutate(x, x2 = x1_lag * 2),
#'   outputs = "x2_lag", inputs = "x1_lag"
#' )
#' feature_pipeline(x1, x2)
#' @export
feature_pipeline <- function(...) {
  nodes <- list(...)
  if (length(nodes) == 1L && is.list(nodes[[1]]) &&
    !inherits(nodes[[1]], "tsbl_feature_node")) {
    nodes <- nodes[[1]]
  }
  if (is_empty(nodes)) {
    abort("A feature pipeline requires at least one `feature_node()`.")
  }
  ok <- map_lgl(nodes, inherits, "tsbl_feature_node")
  if (!all(ok)) {
    abort("Every input to `feature_pipeline()` must be created by `feature_node()`.")
  }
  ids <- map_chr(nodes, `[[`, "name")
  if (anyDuplicated(ids)) {
    dup <- unique(ids[duplicated(ids)])
    abort(sprintf(
      "Feature pipeline contains duplicated node name(s) %s.",
      comma(backticks(dup))
    ))
  }
  names(nodes) <- ids

  node_outputs <- map(nodes, `[[`, "outputs")
  all_outputs <- vec_c(!!!unname(node_outputs))
  if (anyDuplicated(all_outputs)) {
    dup <- unique(all_outputs[duplicated(all_outputs)])
    abort(sprintf(
      "Feature pipeline contains conflicting outputs: %s.",
      comma(backticks(dup))
    ))
  }

  for (id in ids) {
    unknown <- setdiff(nodes[[id]]$depends, ids)
    if (!is_empty(unknown)) {
      abort(sprintf(
        "Feature node `%s` depends on unknown node(s) %s.",
        id, comma(backticks(unknown))
      ))
    }
  }

  pipeline_topo(nodes)
  structure(list(nodes = nodes), class = "tsbl_feature_pipeline")
}

#' @export
print.tsbl_feature_pipeline <- function(x, ...) {
  ord <- pipeline_topo(x$nodes)
  cat_line("<feature pipeline> ", length(ord), " nodes")
  for (id in ord) {
    node <- x$nodes[[id]]
    cat_line(
      " ", id, ": ", feature_chr_print(node$inputs), " -> ",
      feature_chr_print(node$outputs), " (", node$stage, ")"
    )
  }
  invisible(x)
}

output_index <- function(nodes) {
  out <- map(nodes, `[[`, "outputs")
  set_names(rep(names(nodes), lengths(out)), vec_c(!!!unname(out)))
}

# Dependencies of each node: the declared `depends` plus the producers of its
# inputs.
node_deps <- function(nodes) {
  prod <- output_index(nodes)
  out <- map(nodes, function(node) {
    implied <- unname(prod[vec_match(node$inputs, names(prod))])
    implied <- implied[!is.na(implied)]
    vec_unique(c(node$depends, implied))
  })
  set_names(out, names(nodes))
}

# Topological order of the nodes.
pipeline_topo <- function(nodes) {
  ids <- names(nodes)
  if (is_empty(ids)) {
    return(character())
  }
  deps <- node_deps(nodes)
  ordered <- character()
  remaining <- ids
  while (length(remaining) > 0) {
    ready <- remaining[map_lgl(
      remaining, function(id) all(deps[[id]] %in% ordered)
    )]
    if (is_empty(ready)) {
      abort(c(
        "Feature pipeline contains a dependency cycle.",
        i = sprintf("Involved node(s): %s.", comma(backticks(remaining)))
      ))
    }
    ordered <- c(ordered, ready)
    remaining <- setdiff(remaining, ready)
  }
  ordered
}

# Nodes that apply to `stage`, validated against the columns of the table they
# will run on, in dependency order.
select_feature_nodes <- function(pipeline, stage = NULL, source_cols,
                                 response = NULL) {
  all_nodes <- pipeline$nodes
  if (is_null(stage)) {
    sel_ids <- names(all_nodes)
  } else {
    sel_ids <- names(all_nodes)[map_lgl(
      all_nodes, function(node) node$stage %in% c("both", stage)
    )]
  }
  nodes <- all_nodes[sel_ids]
  deps <- node_deps(nodes)
  prod <- output_index(nodes)
  all_prod <- output_index(all_nodes)

  for (id in sel_ids) {
    node <- nodes[[id]]
    bad_dep <- setdiff(deps[[id]], sel_ids)
    if (!is_empty(bad_dep)) {
      abort(c(
        sprintf(
          "Feature node `%s` depends on node(s) %s, which do not run here.",
          id, comma(backticks(bad_dep))
        ),
        i = "Check the `stage` of the dependency."
      ))
    }
    for (input in node$inputs) {
      if (input %in% names(prod) || input %in% source_cols) {
        next
      }
      producer <- all_prod[input]
      if (!is.na(producer)) {
        abort(c(
          sprintf(
            "Feature node `%s` requires `%s`, which is generated by node `%s` outside this stage.",
            id, input, producer
          ),
          i = "A node can only use columns available in the same stage."
        ))
      }
      if (!is_null(response) && identical(input, response)) {
        abort(c(
          sprintf(
            "Feature node `%s` uses the response `%s`, which is not available for prediction.",
            id, input
          ),
          i = sprintf(
            "Set `stage = \"history\"` on node `%s` to compute it from the historical data only.",
            id
          )
        ))
      }
      abort(c(
        sprintf(
          "Feature node `%s` requires `%s`, which is neither a column of the data nor an output of another node.",
          id, input
        ),
        i = "Declare the column as an `input` of a node that produces it."
      ))
    }
    clash <- intersect(node$outputs, source_cols)
    if (!is_empty(clash)) {
      abort(c(
        sprintf(
          "Feature node `%s` produces column(s) %s that already exist in the data.",
          id, comma(backticks(clash))
        ),
        i = "Output columns must not overwrite key, index or existing columns."
      ))
    }
  }
  nodes[pipeline_topo(nodes)]
}

#' Compute the features of a table
#'
#' @description
#' `compute_features()` evaluates a [feature_pipeline()] on `data` and returns a
#' tsibble made of the key, the index, and the outputs of the nodes that apply.
#' Raw columns that are only read by nodes are not carried into the feature
#' table unless a node produces them.
#'
#' Every node runs once, sees only its declared inputs, and its result is
#' matched back on the key and index of `data`. Missing, duplicated or extra
#' key/index rows all abort the computation, so a half-aligned feature table is
#' never returned.
#'
#' @param .data A tsibble.
#' @param pipeline A [feature_pipeline()].
#' @param stage `NULL` (default) runs every node in the pipeline. `"history"`
#' runs the nodes that apply to the historical table, `"future"` those that
#' apply to the future table.
#' @param ... Passed on to individual methods.
#'
#' @return A tsibble holding the key, the index and the pipeline outputs, with
#' the column types, key/index metadata and grouping of `.data`.
#' @seealso [feature_forecast()]
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' tsbl <- tsibble(
#'   id = rep(c("a", "b"), each = 4), t = rep(1:4, 2),
#'   value = 1:8, key = id, index = t
#' )
#' lagged <- feature_node(
#'   "lagged", function(x) mutate(x, value_lag = lag(value)),
#'   outputs = "value_lag", inputs = "value"
#' )
#' compute_features(tsbl, feature_pipeline(lagged))
#' @export
compute_features <- function(.data, pipeline, stage = NULL, ...) {
  UseMethod("compute_features")
}

#' @export
compute_features.tbl_ts <- function(.data, pipeline, stage = NULL, ...) {
  if (!is_null(stage)) {
    stage <- match.arg(stage, c("history", "future"))
  }
  run_feature_stage(.data, pipeline, stage)$features
}

#' @export
compute_features.default <- function(.data, pipeline, stage = NULL, ...) {
  abort("`.data` must be a tsibble.")
}

as_pipeline <- function(pipeline) {
  if (inherits(pipeline, "tsbl_feature_pipeline")) {
    return(pipeline)
  }
  if (inherits(pipeline, "tsbl_feature_node")) {
    return(feature_pipeline(pipeline))
  }
  if (is.list(pipeline) &&
    all(map_lgl(pipeline, inherits, "tsbl_feature_node"))) {
    return(feature_pipeline(pipeline))
  }
  abort("`pipeline` must be created by `feature_pipeline()`.")
}

# Run every applicable node on one table. Returns the feature tsibble together
# with the computed values, which the caller may need for later stages.
run_feature_stage <- function(.data, pipeline, stage = NULL, response = NULL) {
  if (!is_tsibble(.data)) {
    abort("`.data` must be a tsibble.")
  }
  pipeline <- as_pipeline(pipeline)
  nr <- vec_size(.data)
  src <- new_tibble(vec_data(.data), nrow = nr)
  key <- key_vars(.data)
  idx <- index_var(.data)
  idx2 <- index2_var(.data)
  align <- vec_unique(c(key, idx, idx2))
  align_df <- src[align]

  if (vec_size(align_df) != nr) {
    abort(c(
      "The feature pipeline requires distinct key and index values.",
      i = "Please use `duplicates()` to check the duplicated rows."
    ))
  }
  is_missing <- map_lgl(align_df, anyNA)
  if (any(is_missing)) {
    abort(c(
      sprintf(
        "The feature pipeline requires non-missing key/index values in %s.",
        comma(backticks(names(align_df)[is_missing]))
      ),
      i = "Remove or fill the `NA`s before computing features."
    ))
  }

  nodes <- select_feature_nodes(pipeline, stage, names(src), response)
  node_groups <- key
  values <- list()
  produced <- character()

  for (id in names(nodes)) {
    node <- nodes[[id]]
    cols <- vec_unique(c(align, node$inputs))
    node_cols <- set_names(vector("list", length(cols)), cols)
    for (col in cols) {
      val <- values[[col]]
      if (is_null(val)) val <- src[[col]]
      if (is_null(val)) {
        abort(sprintf(
          "Feature node `%s` requires `%s`, which is not available.",
          id, col
        ))
      }
      node_cols[[col]] <- val
    }
    node_data <- as_feature_tsibble(
      new_tibble(node_cols, nrow = nr), .data,
      groups = node_groups, index2 = idx2
    )
    res <- run_feature_node(node, node_data, id)
    res <- validate_feature_result(res, node, align, align_df, nr, id)
    for (out in node$outputs) {
      values[[out]] <- res[[out]]
    }
    produced <- c(produced, node$outputs)
  }

  feat <- new_tibble(
    set_names(
      c(map(align, function(col) src[[col]]), values[produced]),
      c(align, produced)
    ),
    nrow = nr
  )
  groups <- character()
  if (is_grouped_ts(.data)) {
    groups <- group_vars(.data)
    lost <- setdiff(groups, names(feat))
    if (!is_empty(lost)) {
      inform(c(
        sprintf(
          "Grouping column(s) %s are not part of the feature table.",
          comma(backticks(lost))
        ),
        i = "The features are no longer grouped by them."
      ))
      groups <- setdiff(groups, lost)
    }
  }
  list(
    features = as_feature_tsibble(feat, .data, groups = groups),
    values = values[produced],
    nodes = names(nodes),
    align = align
  )
}

# Row order is allowed to change inside a node, so tsibble's ordering warning
# is not actionable here. Everything else is surfaced as usual.
run_feature_node <- function(node, data, id) {
  withCallingHandlers(
    node$fn(data),
    warning = function(w) {
      if (grepl(
        "Current temporal ordering may yield unexpected results",
        conditionMessage(w), fixed = TRUE
      )) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Check a node result and return its columns aligned to the source table.
validate_feature_result <- function(res, node, align, align_df, nr, id) {
  if (!is.data.frame(res)) {
    abort(sprintf(
      "Feature node `%s` must return a data frame, not <%s>.",
      id, class(res)[1]
    ))
  }
  res <- new_tibble(vec_data(res), nrow = vec_size(res))
  res_names <- names(res)

  missing_align <- setdiff(align, res_names)
  if (!is_empty(missing_align)) {
    abort(c(
      sprintf(
        "Feature node `%s` must return the key/index column(s) %s.",
        id, comma(backticks(missing_align))
      ),
      i = "Return the key and index columns so the result can be aligned."
    ))
  }
  unexpected <- setdiff(res_names, c(align, node$inputs, node$outputs))
  if (!is_empty(unexpected)) {
    abort(c(
      sprintf(
        "Feature node `%s` returned undeclared column(s) %s.",
        id, comma(backticks(unexpected))
      ),
      i = "A node may only return its key, index, inputs and declared outputs."
    ))
  }

  res_align <- res[align]
  if (vec_duplicate_any(res_align)) {
    abort(c(
      sprintf("Feature node `%s` returned duplicated key/index rows.", id),
      i = "Please use `duplicates()` to check the duplicated rows."
    ))
  }
  loc <- vec_match(align_df, res_align)
  if (anyNA(loc)) {
    abort(c(
      sprintf(
        "Feature node `%s` is missing %s key/index row(s).",
        id, sum(is.na(loc))
      ),
      i = "Every row of the data must be returned exactly once."
    ))
  }
  if (vec_size(res) != nr) {
    abort(c(
      sprintf(
        "Feature node `%s` returned %s row(s) instead of %s.",
        id, vec_size(res), nr
      ),
      i = "Every row of the data must be returned exactly once."
    ))
  }
  absent <- setdiff(node$outputs, res_names)
  if (!is_empty(absent)) {
    abort(sprintf(
      "Feature node `%s` did not return the declared output(s) %s.",
      id, comma(backticks(absent))
    ))
  }
  out <- set_names(vector("list", length(node$outputs)), node$outputs)
  for (col in node$outputs) {
    out[[col]] <- vec_slice(res[[col]], loc)
  }
  out
}

# Rebuild a tsibble with the key/index metadata of `template`. `x` must hold
# exactly the same key/index rows as `template`, in the same order.
as_feature_tsibble <- function(x, template, groups = key_vars(template),
                               index2 = index2_var(template)) {
  groups <- intersect(groups, names(x))
  if (length(groups) > 0) {
    if (identical(groups, key_vars(template))) {
      x <- new_grouped_df(x, key_data(template))
    } else {
      x <- grouped_df(x, groups)
    }
  }
  build_tsibble_meta(
    x,
    key_data = key_data(template), index = index_var(template),
    index2 = index2, ordered = is_ordered(template),
    interval = interval(template)
  )
}

#' Fit on historical features and forecast future features
#'
#' @description
#' `feature_forecast()` runs a [feature_pipeline()] separately on the historical
#' data and on the future data, then hands the two feature tables to a pair of
#' modelling functions:
#'
#' * `fit(history_features, y, ...)` receives the historical features and the
#' response aligned with them,
#' * `predict(model, future_features, ...)` receives **only** the future
#' features.
#'
#' The tables handed to the model are the very tables returned in `history`,
#' `future` and `forecast`, so what the model sees is what is delivered. The
#' historical table is aligned on its own key and index, and the future table on
#' its own; features are never borrowed across series or across time points.
#'
#' @param .data A tsibble holding the response and the raw inputs.
#' @param pipeline A [feature_pipeline()].
#' @param new_data The future table: a data frame holding the key, the index and
#' any known future inputs, or a number of future observations to generate with
#' [new_data()]. It must not contain the response.
#' @param fit A function of the historical features and the response, as in
#' `fit(history_features, y, ...)`.
#' @param predict A function of the fitted model and the future features, as in
#' `predict(model, future_features, ...)`.
#' @param y The response column in `.data`.
#' @param fit_args,predict_args Lists of additional arguments passed on to
#' `fit` and `predict`.
#' @param pred_name The column name used for the predictions when `predict()`
#' returns a vector.
#'
#' @return A list of class `tsbl_feature_forecast` with elements
#' * `history`: the historical feature tsibble,
#' * `future`: the future feature tsibble,
#' * `forecast`: a tsibble covering every future key/index pair with the
#' predictions,
#' * `model`: the object returned by `fit`,
#' * `features`: a list of the `history` and `future` feature tables,
#' * `pipeline`: the pipeline that was run.
#' @seealso [compute_features()]
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' tsbl <- tsibble(
#'   id = rep(c("a", "b"), each = 6), t = rep(1:6, 2),
#'   y = c(1:6, 3:8), x = c(2:7, 4:9), key = id, index = t
#' )
#' trend <- feature_node(
#'   "trend", function(x) mutate(x, trend = as.numeric(t)),
#'   outputs = "trend", inputs = "t"
#' )
#' future <- new_data(tsbl, n = 2)
#' future <- as_tsibble(future, key = id, index = t)
#' future
#' res <- feature_forecast(
#'   tsbl, feature_pipeline(trend), future,
#'   fit = function(h, y) lm(y ~ trend, data = mutate(as_tibble(h), y = y)),
#'   predict = function(m, f) predict(m, newdata = as_tibble(f)),
#'   y = y
#' )
#' res$forecast
#' @export
feature_forecast <- function(.data, pipeline, new_data, fit, predict, y,
                             fit_args = list(), predict_args = list(),
                             pred_name = ".pred") {
  if (!is_tsibble(.data)) {
    abort("`.data` must be a tsibble.")
  }
  if (missing(y)) {
    abort("Argument `y` must specify the response column of `.data`.")
  }
  if (missing(fit)) {
    abort("Argument `fit` must be a function.")
  }
  if (missing(predict)) {
    abort("Argument `predict` must be a function.")
  }
  y_var <- vars_pull(names(.data), !!enquo(y))
  fit <- as_function(fit)
  predict <- as_function(predict)
  pipeline <- as_pipeline(pipeline)

  future <- normalise_future(new_data, .data, y_var)
  if (is_grouped_ts(.data)) {
    # Carry the historical grouping into the future table, so that both feature
    # tables are grouped the same way.
    groups <- intersect(group_vars(.data), names(future))
    if (!is_empty(groups)) {
      future <- as_feature_tsibble(
        new_tibble(vec_data(future), nrow = vec_size(future)),
        future, groups = groups
      )
    }
  }

  history_run <- run_feature_stage(.data, pipeline, "history", y_var)
  future_run <- run_feature_stage(future, pipeline, "future", y_var)
  history <- history_run$features
  future <- future_run$features

  absent <- setdiff(names(history), names(future))
  if (!is_empty(absent)) {
    inform(c(
      sprintf(
        "Feature(s) %s are not available for prediction.",
        comma(backticks(absent))
      ),
      i = "They are computed from the historical table only."
    ))
  }

  model <- exec(fit, history, .data[[y_var]], !!!fit_args)
  pred <- exec(predict, model, future, !!!predict_args)
  forecast <- build_feature_forecast(pred, future, pred_name)

  structure(
    list(
      history = history, future = future, forecast = forecast,
      model = model, features = list(history = history, future = future),
      pipeline = pipeline
    ),
    class = "tsbl_feature_forecast"
  )
}

#' @export
print.tsbl_feature_forecast <- function(x, ...) {
  cat_line("<tsibble feature forecast>")
  cat_line(" history:  ", dim_tbl_ts(x$history))
  cat_line(" future:   ", dim_tbl_ts(x$future))
  cat_line(" forecast: ", dim_tbl_ts(x$forecast))
  invisible(x)
}

normalise_future <- function(new_data, x, response = NULL) {
  if (is_integerish(new_data) && length(new_data) == 1L) {
    if (new_data < 1L) {
      abort("`new_data` must be a positive number of future observations.")
    }
    if (!identical(index2_var(x), index_var(x))) {
      abort(c(
        "Can't generate the future table for a tsibble with a distinct `index2`.",
        i = sprintf(
          "Supply `new_data` as a data frame holding `%s`, `%s` and `%s`.",
          comma(backticks(key_vars(x))), backticks(index_var(x)),
          backticks(index2_var(x))
        )
      ))
    }
    return(new_data.tbl_ts(ungroup_future(x), n = as.integer(new_data)))
  }
  if (!is.data.frame(new_data)) {
    abort("`new_data` must be a data frame or a number of future observations.")
  }
  key <- key_vars(x)
  idx <- index_var(x)
  idx2 <- index2_var(x)
  need <- vec_unique(c(key, idx, idx2))
  absent <- setdiff(need, names(new_data))
  if (!is_empty(absent)) {
    abort(c(
      sprintf(
        "The future data is missing the key/index column(s) %s.",
        comma(backticks(absent))
      ),
      i = "Use `new_data()` to generate a future table."
    ))
  }
  if (!is_null(response) && response %in% names(new_data)) {
    inform(c(
      sprintf("The response `%s` is not a feature of the future table.", response),
      i = "It is dropped: predictions use future features only."
    ))
    new_data <- new_data[setdiff(names(new_data), response)]
  }
  if (is_tsibble(new_data)) {
    if (!identical(key_vars(new_data), key)) {
      abort(c(
        "The future data has different key variables from the historical data.",
        i = sprintf(
          "Historical: %s. Future: %s.",
          if (is_empty(key)) "(none)" else comma(backticks(key)),
          if (is_empty(key_vars(new_data))) "(none)"
          else comma(backticks(key_vars(new_data)))
        )
      ))
    }
    if (!identical(index_var(new_data), idx)) {
      abort(sprintf(
        "The future data is indexed by `%s`, not `%s`.",
        index_var(new_data), idx
      ))
    }
    new_data <- new_tibble(vec_data(new_data), nrow = vec_size(new_data))
  }
  if (vec_duplicate_any(new_data[need])) {
    abort(c(
      "The future data has duplicated key/index rows.",
      i = "Please use `duplicates()` to check the duplicated rows."
    ))
  }
  is_missing <- map_lgl(new_data[need], anyNA)
  if (any(is_missing)) {
    abort(sprintf(
      "The future data has missing values in %s.",
      comma(backticks(names(is_missing)[is_missing]))
    ))
  }
  build_tsibble(
    new_data, key = !!key, index = !!idx, index2 = !!idx2,
    interval = interval(x), ordered = NULL, validate = TRUE
  )
}

# `new_data()` warns about groups and fails on a distinct `index2`, so the
# future scaffold is generated from an ungrouped copy with `index2` dropped.
ungroup_future <- function(x) {
  out <- build_tsibble(
    new_tibble(vec_data(x), nrow = vec_size(x)),
    key = !!key_vars(x), index = !!index_var(x),
    ordered = is_ordered(x), interval = interval(x), validate = FALSE
  )
  out
}

build_feature_forecast <- function(pred, features, pred_name = ".pred") {
  nr <- vec_size(features)
  align <- vec_unique(c(
    key_vars(features), index_var(features), index2_var(features)
  ))
  align_df <- new_tibble(vec_data(features)[align], nrow = nr)

  if (is.matrix(pred)) {
    pred <- as_tibble(pred)
  }
  if (is.data.frame(pred)) {
    pred <- new_tibble(vec_data(pred), nrow = vec_size(pred))
    if (all(align %in% names(pred))) { # ordered by key/index in the result
      pred_align <- pred[align]
      if (vec_duplicate_any(pred_align)) {
        abort(c(
          "`predict()` returned duplicated key/index rows.",
          i = "Please supply one prediction per future observation."
        ))
      }
      loc <- vec_match(align_df, pred_align)
      if (anyNA(loc)) {
        abort(c(
          "`predict()` must cover every future key/index pair.",
          i = sprintf(
            "%s future observation(s) have no prediction.", sum(is.na(loc))
          )
        ))
      }
      if (vec_size(pred) != nr) {
        abort(c(
          "`predict()` returned more rows than there are future observations.",
          i = sprintf("Expected %s row(s), got %s.", nr, vec_size(pred))
        ))
      }
      pred <- pred[setdiff(names(pred), align)]
      pred <- new_tibble(map(pred, function(col) vec_slice(col, loc)), nrow = nr)
    } else {
      pred <- pred[setdiff(names(pred), align)]
      if (vec_size(pred) != nr) {
        abort(c(
          "`predict()` must return one prediction per future observation.",
          i = sprintf("Expected %s row(s), got %s.", nr, vec_size(pred))
        ))
      }
    }
    if (is_empty(names(pred))) {
      abort("`predict()` returned no prediction columns.")
    }
  } else if (is.atomic(pred) || is.list(pred)) {
    if (vec_size(pred) != nr) {
      abort(c(
        "`predict()` must return one prediction per future observation.",
        i = sprintf("Expected %s value(s), got %s.", nr, vec_size(pred))
      ))
    }
    if (pred_name %in% align) {
      abort(sprintf(
        "`pred_name` must not be a key/index column, but is `%s`.", pred_name
      ))
    }
    pred <- new_tibble(set_names(list(pred), pred_name), nrow = nr)
  } else {
    abort(sprintf(
      "`predict()` must return a vector or a data frame, not <%s>.",
      class(pred)[1]
    ))
  }

  out <- new_tibble(c(as.list(align_df), as.list(pred)), nrow = nr)
  groups <- if (is_grouped_ts(features)) group_vars(features) else character()
  as_feature_tsibble(out, features, groups = groups)
}
