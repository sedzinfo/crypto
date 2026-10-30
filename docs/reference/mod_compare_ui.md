# Compare module - UI

A picker for up to 10 coins, a start date, a button that downloads the
normalized prices as CSV, and a chart of the coins' close prices
normalized to 100.

## Usage

``` r
mod_compare_ui(id, coins, top_coins)
```

## Arguments

- id:

  Character. Module namespace id.

- coins:

  Data frame of coins from
  [`get_coins()`](https://sedzinfo.github.io/golemcrypto/reference/get_coins.md),
  whose `name_symbol` labels fill the picker.

- top_coins:

  Coin labels sorted by market cap; the first three that are in `coins`
  are selected at start.

## Value

A `tagList` of Shiny UI elements.

## Examples

``` r
if (FALSE) { # \dontrun{
coins <- golemcrypto:::get_coins()
shiny::fluidPage(golemcrypto:::mod_compare_ui("compare", coins, c("Bitcoin (BTC)", "Ethereum (ETH)")))
} # }
```
