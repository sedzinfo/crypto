# Timeseries module - UI

A coin picker with live search, a button that downloads the coin's daily
prices as CSV, and a candlestick chart of those prices, with the trading
volume underneath.

## Usage

``` r
mod_timeseries_ui(id, coins)
```

## Arguments

- id:

  Character. Module namespace id.

- coins:

  Data frame of coins from
  [`get_coins()`](https://sedzinfo.github.io/crypto/reference/get_coins.md),
  whose `name_symbol` labels fill the picker.

## Value

A `tagList` of Shiny UI elements.

## Examples

``` r
if (FALSE) { # \dontrun{
shiny::fluidPage(crypto:::mod_timeseries_ui("timeseries", crypto:::get_coins()))
} # }
```
