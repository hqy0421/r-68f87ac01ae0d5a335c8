# Fit on historical features and forecast future features

`feature_forecast()` runs a
[`feature_pipeline()`](https://tsibble.tidyverts.org/dev/reference/feature_pipeline.md)
separately on the historical data and on the future data, then hands the
two feature tables to a pair of modelling functions:

- `fit(history_features, y, ...)` receives the historical features and
  the response aligned with them,

- `predict(model, future_features, ...)` receives **only** the future
  features.

The tables handed to the model are the very tables returned in
`history`, `future` and `forecast`, so what the model sees is what is
delivered. The historical table is aligned on its own key and index, and
the future table on its own; features are never borrowed across series
or across time points.

## Usage

``` r
feature_forecast(
  .data,
  pipeline,
  new_data,
  fit,
  predict,
  y,
  fit_args = list(),
  predict_args = list(),
  pred_name = ".pred"
)
```

## Arguments

- .data:

  A tsibble holding the response and the raw inputs.

- pipeline:

  A
  [`feature_pipeline()`](https://tsibble.tidyverts.org/dev/reference/feature_pipeline.md).

- new_data:

  The future table: a data frame holding the key, the index and any
  known future inputs, or a number of future observations to generate
  with
  [`new_data()`](https://tsibble.tidyverts.org/dev/reference/new-data.md).
  It must not contain the response.

- fit:

  A function of the historical features and the response, as in
  `fit(history_features, y, ...)`.

- predict:

  A function of the fitted model and the future features, as in
  `predict(model, future_features, ...)`.

- y:

  The response column in `.data`.

- fit_args, predict_args:

  Lists of additional arguments passed on to `fit` and `predict`.

- pred_name:

  The column name used for the predictions when
  [`predict()`](https://rdrr.io/r/stats/predict.html) returns a vector.

## Value

A list of class `tsbl_feature_forecast` with elements

- `history`: the historical feature tsibble,

- `future`: the future feature tsibble,

- `forecast`: a tsibble covering every future key/index pair with the
  predictions,

- `model`: the object returned by `fit`,

- `features`: a list of the `history` and `future` feature tables,

- `pipeline`: the pipeline that was run.

## See also

[`compute_features()`](https://tsibble.tidyverts.org/dev/reference/compute_features.md)

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
tsbl <- tsibble(
  id = rep(c("a", "b"), each = 6), t = rep(1:6, 2),
  y = c(1:6, 3:8), x = c(2:7, 4:9), key = id, index = t
)
trend <- feature_node(
  "trend", function(x) mutate(x, trend = as.numeric(t)),
  outputs = "trend", inputs = "t"
)
future <- new_data(tsbl, n = 2)
future <- as_tsibble(future, key = id, index = t)
future
#> # A tsibble: 4 x 2 [1]
#> # Key:       id [2]
#>   id        t
#>   <chr> <dbl>
#> 1 a         7
#> 2 a         8
#> 3 b         7
#> 4 b         8
res <- feature_forecast(
  tsbl, feature_pipeline(trend), future,
  fit = function(h, y) lm(y ~ trend, data = mutate(as_tibble(h), y = y)),
  predict = function(m, f) predict(m, newdata = as_tibble(f)),
  y = y
)
res$forecast
#> # A tsibble: 4 x 3 [1]
#> # Key:       id [2]
#>   id        t .pred
#>   <chr> <dbl> <dbl>
#> 1 a         7     8
#> 2 a         8     9
#> 3 b         7     8
#> 4 b         8     9
```
