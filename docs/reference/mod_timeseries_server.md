# Timeseries module - server

Downloads the full price history of the selected coin, renders its
candlestick chart and serves it as a CSV file named like
`bitcoin-btc-history-2026-10-24.csv`, with the columns `date`,
`name_symbol`, `open`, `high`, `low`, `close`, `volume` and
`market_cap`.

## Usage

``` r
mod_timeseries_server(id, coins, dark_mode)
```

## Arguments

- id:

  Character. Module namespace id, must match the id used in
  [`mod_timeseries_ui()`](https://sedzinfo.github.io/golemcrypto/reference/mod_timeseries_ui.md).

- coins:

  Data frame of coins from
  [`get_coins()`](https://sedzinfo.github.io/golemcrypto/reference/get_coins.md).

- dark_mode:

  Reactive returning `"light"` or `"dark"`, the mode of the app.

## Value

A reactive returning the price history of the selected coin, from
[`get_history()`](https://sedzinfo.github.io/golemcrypto/reference/get_history.md),
which the Returns calendar reuses.

## Examples

``` r
if (FALSE) { # \dontrun{
# inside app_server():
function(input, output, session) {
  history <- mod_timeseries_server("timeseries", coins, reactive(input$dark_mode))
}
} # }
```
