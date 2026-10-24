# Candlestick chart of a coin's prices

Plots the daily open, high, low and close prices, with a line through
the close, its moving averages, date range buttons and a range slider.
Underneath, smaller charts show the trading volume and the drawdown: how
far the price is below its all-time high so far. A click on a legend
entry hides or shows that line.

## Usage

``` r
plot_timeseries(
  history,
  mode = "light",
  averages = c(50, 200),
  show_drawdown = TRUE
)
```

## Arguments

- history:

  Price history of one coin from
  [`get_history()`](https://sedzinfo.github.io/crypto/reference/get_history.md).
  The averages and the drawdown use the whole history, so pass all of it
  and zoom with the range buttons.

- mode:

  `"light"` (the default) or `"dark"`, to suit a light or dark page.

- averages:

  Days of the moving averages to draw (see
  [`moving_average()`](https://sedzinfo.github.io/crypto/reference/moving_average.md)),
  or `NULL` for none.

- show_drawdown:

  `TRUE` to add the drawdown chart (see
  [`drawdown()`](https://sedzinfo.github.io/crypto/reference/drawdown.md)).

## Value

A plotly object.

## Examples

``` r
if (FALSE) { # \dontrun{
btc <- get_history("Bitcoin (BTC)", as.Date("2013-04-29"))
plot_timeseries(btc)

# shorter averages, and no drawdown chart
plot_timeseries(btc, averages = c(20, 100), show_drawdown = FALSE)
} # }
```
