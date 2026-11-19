# Declarative feature pipelines

`feature_pipeline()` bundles
[`feature_node()`](https://tsibble.tidyverts.org/dev/reference/feature_node.md)s
into a directed acyclic graph. The pipeline is only a specification: no
data is touched until
[`compute_features()`](https://tsibble.tidyverts.org/dev/reference/compute_features.md)
or
[`feature_forecast()`](https://tsibble.tidyverts.org/dev/reference/feature_forecast.md)
is called.

Building a pipeline validates the graph. Duplicated node names,
duplicated output columns, unknown dependencies and dependency cycles
all fail immediately.

## Usage

``` r
feature_pipeline(...)
```

## Arguments

- ...:

  [`feature_node()`](https://tsibble.tidyverts.org/dev/reference/feature_node.md)
  specifications, or a single list of them.

## Value

A feature pipeline specification.

## See also

[`feature_node()`](https://tsibble.tidyverts.org/dev/reference/feature_node.md),
[`compute_features()`](https://tsibble.tidyverts.org/dev/reference/compute_features.md),
[`feature_forecast()`](https://tsibble.tidyverts.org/dev/reference/feature_forecast.md)

## Examples

``` r
x1 <- feature_node(
  "x1", function(x) dplyr::mutate(x, x1 = dplyr::lag(x)),
  outputs = "x1_lag", inputs = "x"
)
x2 <- feature_node(
  "x2", function(x) dplyr::mutate(x, x2 = x1_lag * 2),
  outputs = "x2_lag", inputs = "x1_lag"
)
feature_pipeline(x1, x2)
#> <feature pipeline> 2 nodes
#>  x1: `x` -> `x1_lag` (both)
#>  x2: `x1_lag` -> `x2_lag` (both)
```
