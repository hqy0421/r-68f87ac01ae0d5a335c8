# Compute the features of a table

`compute_features()` evaluates a
[`feature_pipeline()`](https://tsibble.tidyverts.org/dev/reference/feature_pipeline.md)
on `data` and returns a tsibble made of the key, the index, and the
outputs of the nodes that apply. Raw columns that are only read by nodes
are not carried into the feature table unless a node produces them.

Every node runs once, sees only its declared inputs, and its result is
matched back on the key and index of `data`. Missing, duplicated or
extra key/index rows all abort the computation, so a half-aligned
feature table is never returned.

## Usage

``` r
compute_features(.data, pipeline, stage = NULL, ...)

# S3 method for class 'tbl_ts'
compute_features(.data, pipeline, stage = NULL, ...)

# Default S3 method
compute_features(.data, pipeline, stage = NULL, ...)
```

## Arguments

- .data:

  A tsibble.

- pipeline:

  A
  [`feature_pipeline()`](https://tsibble.tidyverts.org/dev/reference/feature_pipeline.md).

- stage:

  `NULL` (default) runs every node in the pipeline. `"history"` runs the
  nodes that apply to the historical table, `"future"` those that apply
  to the future table.

- ...:

  Passed on to individual methods.

## Value

A tsibble holding the key, the index and the pipeline outputs, with the
column types, key/index metadata and grouping of `.data`.

## See also

[`feature_forecast()`](https://tsibble.tidyverts.org/dev/reference/feature_forecast.md)

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
tsbl <- tsibble(
  id = rep(c("a", "b"), each = 4), t = rep(1:4, 2),
  value = 1:8, key = id, index = t
)
lagged <- feature_node(
  "lagged", function(x) mutate(x, value_lag = lag(value)),
  outputs = "value_lag", inputs = "value"
)
compute_features(tsbl, feature_pipeline(lagged))
#> # A tsibble: 8 x 3 [1]
#> # Key:       id [2]
#>   id        t value_lag
#>   <chr> <int>     <int>
#> 1 a         1        NA
#> 2 a         2         1
#> 3 a         3         2
#> 4 a         4         3
#> 5 b         1        NA
#> 6 b         2         5
#> 7 b         3         6
#> 8 b         4         7
```
