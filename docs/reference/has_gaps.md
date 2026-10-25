# Check a price history for missing days

When 'CoinMarketCap' blocks the same request twice, crypto2 leaves its
400 days out of the download, which shows as a gap in the dates of a
coin.

## Usage

``` r
has_gaps(history)
```

## Arguments

- history:

  Price history from
  [`download_history()`](https://sedzinfo.github.io/golemcrypto/reference/download_history.md),
  sorted by coin and date.

## Value

`TRUE` when a day is missing between the first and the last day of any
coin.

## Examples

``` r
history <- data.frame(
  name_symbol = "Alpha (A)",
  timestamp = as.Date(c("2024-01-01", "2024-01-02", "2024-01-05"))
)
crypto:::has_gaps(history)
#> [1] TRUE
```
