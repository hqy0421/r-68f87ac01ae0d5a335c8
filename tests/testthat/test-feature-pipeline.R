library(dplyr)

make_panel <- function(n = 5) {
  tsibble(
    id = rep(c("a", "b"), each = n),
    t = rep(seq_len(n), 2),
    y = c(seq_len(n), seq_len(n) * 2L),
    x = c(seq_len(n) * 10L, seq_len(n) * 20L),
    key = id, index = t
  )
}

trivial_node <- function(value = 1) {
  feature_node(
    "const", function(d) mutate(d, const = value),
    outputs = "const"
  )
}

test_that("a node only sees its declared inputs", {
  tsbl <- make_panel()
  seen <- NULL
  probe <- feature_node(
    "probe", function(d) {
      seen <<- names(d)
      mutate(d, z = x + 1L)
    },
    outputs = "z", inputs = "x"
  )
  out <- compute_features(tsbl, feature_pipeline(probe))
  expect_identical(seen, c("id", "t", "x"))
  expect_identical(names(out), c("id", "t", "z"))
  expect_true(is_tsibble(out))
})

test_that("nodes run in dependency order and shared nodes once", {
  tsbl <- make_panel()
  calls <- c(left = 0L, right = 0L, shared = 0L)
  shared <- feature_node(
    "shared", function(d) {
      calls[["shared"]] <<- calls[["shared"]] + 1L
      mutate(d, s = x * 2L)
    },
    outputs = "s", inputs = "x"
  )
  left <- feature_node(
    "left", function(d) {
      calls[["left"]] <<- calls[["left"]] + 1L
      mutate(d, l = s + 1L)
    },
    outputs = "l", inputs = "s"
  )
  right <- feature_node(
    "right", function(d) {
      calls[["right"]] <<- calls[["right"]] + 1L
      mutate(d, r = s - 1L)
    },
    outputs = "r", inputs = "s"
  )
  out <- compute_features(
    tsbl, feature_pipeline(left, right, shared)
  )
  expect_identical(unname(calls), c(1L, 1L, 1L))
  expect_identical(out$s, tsbl$x * 2L)
  expect_identical(out$l, out$s + 1L)
  expect_identical(out$r, out$s - 1L)
})

test_that("a node may reorder its rows", {
  tsbl <- make_panel(6)
  rev_node <- feature_node(
    "rev", function(d) {
      d <- d[rev(seq_len(nrow(d))), ]
      mutate(d, doubled = x * 2L)
    },
    outputs = "doubled", inputs = "x"
  )
  out <- compute_features(tsbl, feature_pipeline(rev_node))
  expect_identical(out$doubled, tsbl$x * 2L)
  expect_identical(key_vars(out), "id")
  expect_identical(index_var(out), "t")
  expect_identical(out$id, tsbl$id)
  expect_identical(out$t, tsbl$t)
})

test_that("missing, duplicated and extra keys fail explicitly", {
  tsbl <- make_panel(4)
  drop_node <- feature_node(
    "drop", function(d) {
      mutate(slice(d, -1L), v = x)
    },
    outputs = "v", inputs = "x"
  )
  expect_error(
    compute_features(tsbl, feature_pipeline(drop_node)),
    "missing 2 key/index row"
  )

  dup_node <- feature_node(
    "dup", function(d) {
      dup <- as_tibble(d)[c("id", "t")]
      dup$v <- d$x
      bind_rows(dup, dup[1, ])
    },
    outputs = "v", inputs = "x"
  )
  expect_error(
    compute_features(tsbl, feature_pipeline(dup_node)),
    "duplicated key/index rows"
  )

  extra <- tibble(
    id = c("a", tsbl$id), t = c(99, tsbl$t),
    v = c(0L, tsbl$x)
  )
  extra_node <- feature_node(
    "extra", function(d) extra, outputs = "v", inputs = "x"
  )
  expect_error(
    compute_features(tsbl, feature_pipeline(extra_node)),
    "row\\(s\\) instead of"
  )
})

test_that("undeclared outputs and column clashing fail explicitly", {
  tsbl <- make_panel()
  sneaky <- feature_node(
    "sneaky", function(d) mutate(d, v = x, w = x),
    outputs = "v", inputs = "x"
  )
  expect_error(
    compute_features(tsbl, feature_pipeline(sneaky)),
    "undeclared column"
  )

  clash <- feature_node(
    "clash", function(d) mutate(d, x = x + 1L), outputs = "x", inputs = "y"
  )
  expect_error(
    compute_features(tsbl, feature_pipeline(clash)),
    "already exist in the data"
  )

  missing_key <- feature_node(
    "nokey", function(d) tibble(v = d$x), outputs = "v", inputs = "x"
  )
  expect_error(
    compute_features(tsbl, feature_pipeline(missing_key)),
    "key/index column"
  )
})

test_that("unknown dependencies and cycles fail at construction", {
  a <- feature_node(
    "a", function(d) mutate(d, av = x), outputs = "av",
    inputs = "x", depends = "b"
  )
  b <- feature_node(
    "b", function(d) mutate(d, bv = x), outputs = "bv",
    inputs = "x", depends = "a"
  )
  expect_error(feature_pipeline(a, b), "dependency cycle")

  lonely <- feature_node(
    "lonely", function(d) mutate(d, lv = x), outputs = "lv",
    inputs = "x", depends = "nobody"
  )
  expect_error(feature_pipeline(lonely), "unknown node")

  dup_name <- trivial_node()
  dup_name2 <- feature_node(
    "const", function(d) mutate(d, other = 1), outputs = "other"
  )
  expect_error(feature_pipeline(dup_name, dup_name2), "duplicated node name")

  conflict_a <- feature_node(
    "ca", function(d) mutate(d, out = 1), outputs = "out"
  )
  conflict_b <- feature_node(
    "cb", function(d) mutate(d, out = 2), outputs = "out"
  )
  expect_error(feature_pipeline(conflict_a, conflict_b), "conflicting outputs")
})

test_that("node specifications are validated", {
  expect_error(
    feature_node("n", function(d) d, outputs = c("a", "a")),
    "duplicated output"
  )
  expect_error(
    feature_node("n", function(d) d, outputs = "a", inputs = "a"),
    "both consume and produce"
  )
  expect_error(feature_node("n", function(d) d), "must declare")
  expect_error(
    feature_node("", function(d) d, outputs = "a"), "non-empty string"
  )
})

test_that("columns, key, index and grouping are preserved", {
  tsbl <- tsibble(
    id = rep(c("a", "b"), each = 4),
    t = rep(1:4, 2),
    x = c(1:4, 5:8),
    key = id, index = t
  )
  node <- feature_node(
    "typed", function(d) mutate(d, when = as.Date("2020-01-01") + t),
    outputs = "when", inputs = "t"
  )
  out <- compute_features(tsbl, feature_pipeline(node))
  expect_identical(out$id, tsbl$id)
  expect_identical(out$t, tsbl$t)
  expect_s3_class(out$when, "Date")
  expect_identical(key_data(out), key_data(tsbl))
  expect_identical(index_var(out), "t")
  expect_identical(interval(out), interval(tsbl))

  grouped <- group_by_key(tsbl)
  gout <- compute_features(grouped, feature_pipeline(node))
  expect_true(is_grouped_ts(gout))
  expect_identical(group_vars(gout), "id")
  expect_identical(group_data(gout), group_data(grouped))
})

test_that("a failing stage returns nothing", {
  tsbl <- make_panel()
  ok <- feature_node("ok", function(d) mutate(d, a = x), outputs = "a", inputs = "x")
  bad <- feature_node(
    "bad", function(d) mutate(d, b = a), outputs = "b", inputs = "a"
  )
  # `a` is available, so this succeeds ...
  expect_s3_class(
    compute_features(tsbl, feature_pipeline(ok, bad)), "tbl_ts"
  )
  boom <- feature_node("boom", function(d) stop("kaboom"), outputs = "c")
  pipeline <- feature_pipeline(ok, boom)
  expect_error(
    compute_features(tsbl, pipeline),
    "kaboom",
    class = "simpleError"
  )

  # A failing future stage must not hand back the history-only result either.
  future <- tsibble(
    id = c("a", "b"), t = c(6, 6), x = c(60L, 120L), key = id, index = t
  )
  future_boom <- feature_node(
    "future_boom", function(d) stop("kaboom"), outputs = "c", stage = "future"
  )
  outcome <- tryCatch(
    feature_forecast(
      tsbl, feature_pipeline(ok, future_boom), future,
      fit = function(history, y) NULL,
      predict = function(model, new_data) 0,
      y = y
    ),
    error = function(e) "failed"
  )
  expect_identical(outcome, "failed")
  expect_s3_class(
    feature_forecast(
      tsbl, feature_pipeline(ok), future,
      fit = function(history, y) NULL,
      predict = function(model, new_data) rep(0, vctrs::vec_size(new_data)),
      y = y
    ),
    "tsbl_feature_forecast"
  )
})

test_that("fit gets history + y and predict gets only future features", {
  tsbl <- make_panel(6)
  hist_only <- feature_node(
    "hist_only", function(d) mutate(d, y_lag = lag(y)),
    outputs = "y_lag", inputs = "y", stage = "history"
  )
  trend <- feature_node(
    "trend", function(d) mutate(d, trend = as.numeric(t)),
    outputs = "trend", inputs = "t"
  )
  pipeline <- feature_pipeline(hist_only, trend)
  future <- new_data(tsbl, n = 2)
  future <- as_tsibble(future, key = id, index = t)

  seen_fit <- NULL
  seen_pred <- NULL
  res <- feature_forecast(
    tsbl, pipeline, future,
    fit = function(history, y) {
      seen_fit <<- list(features = history, y = y)
      lm(y ~ trend, data = mutate(as_tibble(history), y = y))
    },
    predict = function(model, new_data) {
      seen_pred <<- new_data
      predict(model, newdata = as_tibble(new_data))
    },
    y = y
  )

  expect_identical(seen_fit$features, res$history)
  expect_identical(seen_fit$y, tsbl$y)
  expect_identical(seen_pred, res$future)
  expect_identical(names(res$future), c("id", "t", "trend"))
  expect_false("y" %in% names(res$future))
  expect_false("y_lag" %in% names(res$future))
  expect_true(is_tsibble(res$history))
  expect_true(is_tsibble(res$future))
  expect_true(is_tsibble(res$forecast))
})

test_that("the forecast covers every future key/index pair exactly once", {
  tsbl <- make_panel(5)
  trend <- feature_node(
    "trend", function(d) mutate(d, trend = as.numeric(t)),
    outputs = "trend", inputs = "t"
  )
  future <- as_tsibble(new_data(tsbl, n = 3), key = id, index = t)
  res <- feature_forecast(
    tsbl, feature_pipeline(trend), future,
    fit = function(history, y) {
      lm(y ~ trend, data = mutate(as_tibble(history), y = y))
    },
    predict = function(model, new_data) predict(model, as_tibble(new_data)),
    y = y
  )
  expect_identical(res$forecast$id, future$id)
  expect_identical(res$forecast$t, future$t)
  expect_identical(vctrs::vec_size(res$forecast), vctrs::vec_size(future))
  expect_identical(key_vars(res$forecast), key_vars(future))
  expect_identical(index_var(res$forecast), "t")
  expect_identical(interval(res$forecast), interval(future))
  expect_equal(
    unname(res$forecast$.pred),
    unname(predict(res$model, newdata = as_tibble(res$future))),
    ignore_attr = TRUE
  )
})

test_that("the response can't leak into prediction features", {
  tsbl <- make_panel(4)
  leaky <- feature_node(
    "leaky", function(d) mutate(d, y2 = y * 2), outputs = "y2", inputs = "y"
  )
  future <- as_tsibble(new_data(tsbl, n = 1), key = id, index = t)
  expect_error(
    feature_forecast(
      tsbl, feature_pipeline(leaky), future,
      fit = function(history, y) NULL,
      predict = function(model, new_data) rep(0, nrow(new_data)),
      y = y
    ),
    "not available for prediction"
  )
  # ... but a history-only lag of the response is fine.
  lag_y <- feature_node(
    "lag_y", function(d) mutate(d, y_lag = lag(y)),
    outputs = "y_lag", inputs = "y", stage = "history"
  )
  res <- NULL
  expect_message(
    res <- feature_forecast(
      tsbl, feature_pipeline(lag_y), future,
      fit = function(history, y) mean(y),
      predict = function(model, new_data) rep(model, nrow(new_data)),
      y = y
    ),
    "not available for prediction"
  )
  expect_identical(res$forecast$.pred, rep(mean(tsbl$y), 2))
})

test_that("predictions are checked for full future coverage", {
  tsbl <- make_panel(4)
  node <- trivial_node()
  future <- as_tsibble(new_data(tsbl, n = 2), key = id, index = t)

  short <- function(model, new_data) rep(0, nrow(new_data) - 1L)
  expect_error(
    feature_forecast(
      tsbl, feature_pipeline(node), future,
      fit = function(history, y) NULL, predict = short, y = y
    ),
    "one prediction per future observation"
  )

  partial <- function(model, new_data) {
    tibble(id = new_data$id[-1], t = new_data$t[-1], .pred = 0)
  }
  expect_error(
    feature_forecast(
      tsbl, feature_pipeline(node), future,
      fit = function(history, y) NULL, predict = partial, y = y
    ),
    "cover every future key/index pair"
  )

  reordered <- function(model, new_data) {
    tibble(
      id = rev(new_data$id), t = rev(new_data$t), .pred = seq_len(nrow(new_data))
    )
  }
  res <- feature_forecast(
    tsbl, feature_pipeline(node), future,
    fit = function(history, y) NULL, predict = reordered, y = y
  )
  expect_identical(res$forecast$id, future$id)
  expect_identical(res$forecast$t, future$t)
  # rows come back in the future table's own key/index order
  expect_identical(res$forecast$.pred, c(4L, 3L, 2L, 1L))
})

test_that("the future table is aligned on its own key/index", {
  tsbl <- make_panel(4)
  lag_x <- feature_node(
    "lag_x", function(d) mutate(d, x_lag = lag(x)), outputs = "x_lag",
    inputs = "x"
  )
  future <- tsibble(
    id = rep(c("a", "b"), each = 2), t = rep(5:6, 2),
    x = c(50L, 60L, 100L, 120L), key = id, index = t
  )
  out <- compute_features(future, feature_pipeline(lag_x))
  expect_identical(out$id, future$id)
  expect_identical(out$t, future$t)
  # The lag is taken within the future table, never borrowed from history.
  expect_identical(out$x_lag, c(NA, 50L, NA, 100L))

  res <- feature_forecast(
    tsbl, feature_pipeline(lag_x), future,
    fit = function(history, y) NULL,
    predict = function(model, new_data) {
      expected <- c(NA_integer_, 50L, NA_integer_, 100L)
      if (!identical(new_data$x_lag, expected)) stop("borrowed from history")
      rep(1, vctrs::vec_size(new_data))
    },
    y = y
  )
  expect_identical(res$forecast$.pred, rep(1, vctrs::vec_size(future)))
})

test_that("stages are resolved per node", {
  tsbl <- make_panel(4)
  both <- trivial_node()
  fwd <- feature_node(
    "fwd", function(d) mutate(d, f = 1), outputs = "f", stage = "future"
  )
  hist <- feature_node(
    "hist", function(d) mutate(d, h = 1), outputs = "h", stage = "history"
  )
  pipeline <- feature_pipeline(both, fwd, hist)
  future <- as_tsibble(new_data(tsbl, n = 1), key = id, index = t)

  expect_identical(
    names(compute_features(tsbl, pipeline)),
    c("id", "t", "const", "f", "h")
  )
  expect_identical(
    names(compute_features(tsbl, pipeline, stage = "history")),
    c("id", "t", "const", "h")
  )
  expect_identical(
    names(compute_features(future, pipeline, stage = "future")),
    c("id", "t", "const", "f")
  )

  dependent <- feature_node(
    "dep", function(d) mutate(d, dep = h + 1), outputs = "dep", inputs = "h"
  )
  expect_error(
    compute_features(future, feature_pipeline(hist, dependent), stage = "future"),
    "outside this stage"
  )
})

test_that("output types survive the round trip", {
  tsbl <- make_panel(3)
  node <- feature_node(
    "types",
    function(d) mutate(
      d, i = as.integer(x), dbl = as.double(x),
      chr = as.character(id), lgl = x > 0, dt = as.Date("2000-01-01") + t
    ),
    outputs = c("i", "dbl", "chr", "lgl", "dt"), inputs = c("x", "t", "id")
  )
  out <- compute_features(tsbl, feature_pipeline(node))
  expect_type(out$i, "integer")
  expect_type(out$dbl, "double")
  expect_type(out$chr, "character")
  expect_type(out$lgl, "logical")
  expect_s3_class(out$dt, "Date")
  expect_identical(out$dt, as.Date("2000-01-01") + tsbl$t)
})

test_that("existing tsibble behaviour is untouched", {
  tsbl <- make_panel(5)
  node <- trivial_node()
  out <- compute_features(tsbl, feature_pipeline(node))
  expect_s3_class(out, "tbl_ts")
  expect_identical(dim(out), c(10L, 3L))
  expect_identical(
    as_tibble(out),
    tibble(id = tsbl$id, t = tsbl$t, const = rep(1, 10))
  )
  expect_identical(
    compute_features(tsbl, feature_pipeline(node)),
    out
  )
  expect_error(compute_features(as_tibble(tsbl), feature_pipeline(node)),
    "must be a tsibble"
  )
})

test_that("grouping survives the forecast round trip", {
  tsbl <- group_by_key(make_panel(4))
  node <- trivial_node()
  future <- as_tsibble(new_data(tsbl, n = 1), key = id, index = t)
  seen <- NULL
  res <- feature_forecast(
    tsbl, feature_pipeline(node), future,
    fit = function(history, y) {
      seen <<- list(groups = group_vars(history), n = length(y))
      NULL
    },
    predict = function(model, new_data) {
      expect_true(is_grouped_ts(new_data))
      rep(0, vctrs::vec_size(new_data))
    },
    y = y
  )
  expect_identical(seen$groups, "id")
  expect_identical(seen$n, 8L)
  expect_true(is_grouped_ts(res$history))
  expect_true(is_grouped_ts(res$future))
  expect_true(is_grouped_ts(res$forecast))
  expect_identical(group_vars(res$forecast), "id")
})

test_that("a distinct index2 is aligned and preserved", {
  idx <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = 3)
  tsbl <- tsibble(
    region = "a", grp = rep(c("x", "y"), each = 3), date = rep(idx, 2),
    y = 1:6, x = 11:16, key = c(region, grp), index = date
  )
  node <- feature_node(
    "n", function(d) mutate(d, z = x * 2L), outputs = "z", inputs = "x"
  )
  out <- compute_features(tsbl, feature_pipeline(node))
  expect_identical(index2_var(out), "date")
  expect_identical(out$date, tsbl$date)
  expect_identical(key_data(out), key_data(tsbl))
  expect_identical(out$z, tsbl$x * 2L)

  future <- tsibble(
    region = "a", grp = rep(c("x", "y"), each = 2),
    date = rep(idx[2:3], 2), x = c(21L, 22L, 31L, 32L),
    key = c(region, grp), index = date
  )
  res <- feature_forecast(
    tsbl, feature_pipeline(node), future,
    fit = function(history, y) NULL,
    predict = function(model, new_data) new_data$z,
    y = y
  )
  expect_identical(res$forecast$date, future$date)
  expect_identical(res$forecast$grp, future$grp)
  expect_identical(res$forecast$.pred, future$x * 2L)
})

test_that("the response may be given by name", {
  tsbl <- make_panel(4)
  node <- trivial_node()
  future <- tsibble(
    id = c("a", "b"), t = c(5, 5), key = id, index = t
  )
  res <- feature_forecast(
    tsbl, feature_pipeline(node), future,
    fit = function(history, y) NULL,
    predict = function(model, new_data) rep(0, vctrs::vec_size(new_data)),
    y = "y"
  )
  expect_identical(res$forecast$.pred, c(0, 0))
})

test_that("invalid future tables fail explicitly", {
  tsbl <- make_panel(4)
  node <- trivial_node()
  expect_error(
    feature_forecast(
      tsbl, feature_pipeline(node), tibble(id = "a"),
      fit = function(history, y) NULL, predict = function(m, f) 0, y = y
    ),
    "missing the key/index"
  )
  dup <- tibble(id = c("a", "a"), t = c(5, 5))
  expect_error(
    feature_forecast(
      tsbl, feature_pipeline(node), dup,
      fit = function(history, y) NULL, predict = function(m, f) 0, y = y
    ),
    "duplicated key/index"
  )
  no_y <- tibble(id = c("a", "b"), t = c(5, 5), y = c(1, 2))
  expect_message(
    feature_forecast(
      tsbl, feature_pipeline(node), no_y,
      fit = function(history, y) NULL,
      predict = function(model, new_data) {
        expect_false("y" %in% names(new_data))
        rep(0, vctrs::vec_size(new_data))
      },
      y = y
    ),
    "is not a feature of the future table"
  )
  expect_error(
    feature_forecast(
      tsbl, feature_pipeline(node), 0,
      fit = function(history, y) NULL, predict = function(m, f) 0, y = y
    ),
    "positive number"
  )
})

test_that("print methods show the specification", {
  node <- trivial_node()
  expect_output(print(node), "<feature node> const")
  expect_output(print(feature_pipeline(node)), "<feature pipeline> 1 nodes")
  tsbl <- make_panel(3)
  future <- tsibble(id = c("a", "b"), t = c(4, 4), key = id, index = t)
  res <- feature_forecast(
    tsbl, feature_pipeline(node), future,
    fit = function(history, y) NULL,
    predict = function(model, new_data) rep(0, vctrs::vec_size(new_data)),
    y = y
  )
  expect_output(print(res), "<tsibble feature forecast>")
})

test_that("prediction names are checked against the key/index", {
  tsbl <- make_panel(3)
  node <- trivial_node()
  future <- tsibble(id = c("a", "b"), t = c(4, 4), key = id, index = t)
  expect_error(
    feature_forecast(
      tsbl, feature_pipeline(node), future,
      fit = function(history, y) NULL,
      predict = function(model, new_data) rep(0, vctrs::vec_size(new_data)),
      y = y, pred_name = "t"
    ),
    "must not be a key/index column"
  )
  # feature columns returned by `predict()` are carried into the forecast
  res <- feature_forecast(
    tsbl, feature_pipeline(node), future,
    fit = function(history, y) NULL,
    predict = function(model, new_data) {
      tibble(id = new_data$id, t = new_data$t, extra = 0)
    },
    y = y
  )
  expect_identical(names(res$forecast), c("id", "t", "extra"))
})
