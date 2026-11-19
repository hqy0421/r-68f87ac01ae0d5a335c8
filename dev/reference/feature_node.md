# Declarative feature nodes

A feature node is a single, self-contained step of a
[`feature_pipeline()`](https://tsibble.tidyverts.org/dev/reference/feature_pipeline.md).
It declares

- `inputs`: the columns it is allowed to read,

- `outputs`: the columns it produces,

- `depends`: the nodes that must run before it.

The node function `fn` receives a tsibble that holds the key and index
columns plus **only** the declared `inputs`, grouped by the key so that
time-series operations such as
[`dplyr::lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html)
stay within each series. It must return a data frame holding the key,
the index and its `outputs`. The returned rows may be in any order: they
are matched back on key and index.

## Usage

``` r
feature_node(
  name,
  fn,
  outputs,
  inputs = character(),
  depends = character(),
  stage = c("both", "history", "future")
)
```

## Arguments

- name:

  A single string identifying the node. Other nodes refer to it in their
  `depends`.

- fn:

  A function (or purrr-style lambda) that computes the node. It takes a
  single tsibble and returns a data frame.

- outputs:

  A character vector of the column names `fn` produces.

- inputs:

  A character vector of the column names `fn` is allowed to read.

- depends:

  A character vector of node names that must be computed first.
  Dependencies implied by an `input` matching another node's `outputs`
  are added automatically.

- stage:

  One of `"both"` (default), `"history"` or `"future"`, indicating the
  table(s) the node applies to. Only `"both"` and `"history"` nodes run
  on the historical table; only `"both"` and `"future"` nodes run on the
  future table.

## Value

A feature node specification.

## See also

[`feature_pipeline()`](https://tsibble.tidyverts.org/dev/reference/feature_pipeline.md),
[`compute_features()`](https://tsibble.tidyverts.org/dev/reference/compute_features.md),
[`feature_forecast()`](https://tsibble.tidyverts.org/dev/reference/feature_forecast.md)

## Examples

``` r
# A lagged input, computed per series
lag_x <- feature_node(
  "lag_x", function(x) dplyr::mutate(x, x_lag = dplyr::lag(x)),
  outputs = "x_lag", inputs = "x"
)
lag_x
#> <feature node> lag_x (both)
#>  inputs:  `x`
#>  outputs: `x_lag`
#>  depends: (none)
```
