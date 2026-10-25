# Line chart of normalized close prices

One line per coin, with buttons that switch the y axis between a linear
and a log scale.

## Usage

``` r
plot_compare(prices, mode = "light")
```

## Arguments

- prices:

  Prices from
  [`normalize_prices()`](https://sedzinfo.github.io/golemcrypto/reference/normalize_prices.md).

- mode:

  `"light"` (the default) or `"dark"`, to suit a light or dark page.

## Value

A plotly object.

## Examples

``` r
if (FALSE) { # \dontrun{
history <- get_history(c("Bitcoin (BTC)", "Ethereum (ETH)", "Solana (SOL)"))
plot_compare(normalize_prices(history))
} # }
```
