# Label coins as "Name (SYMBOL)"

The label identifies coins throughout the package: in the arguments of
[`get_history()`](https://sedzinfo.github.io/golemcrypto/reference/get_history.md),
and in the pickers, tables and plots of the app.

## Usage

``` r
coin_label(coins)
```

## Arguments

- coins:

  Data frame with `name` and `symbol` columns.

## Value

A character vector with one label per row of `coins`.

## Examples

``` r
crypto:::coin_label(data.frame(name = c("Bitcoin", "Ethereum"), symbol = c("BTC", "ETH")))
#> [1] "Bitcoin (BTC)"  "Ethereum (ETH)"
```
