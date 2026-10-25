# Monthly returns of a coin

Each month's return is its last close relative to the last close of the
month before, in percent. The first month, and the months still to come,
are `NA`.

## Usage

``` r
monthly_returns(history)
```

## Arguments

- history:

  Price history of one coin from
  [`get_history()`](https://sedzinfo.github.io/golemcrypto/reference/get_history.md),
  with `timestamp` and `close` columns, sorted by date.

## Value

A year by month matrix of returns in percent, rounded to 2 decimals,
with the years as row names and `month.abb` as column names.

## Examples

``` r
history <- data.frame(
  timestamp = as.Date(c("2024-01-31", "2024-02-29", "2024-03-31")),
  close = c(100, 110, 121)
)
monthly_returns(history)
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 2024  NA  10  10  NA  NA  NA  NA  NA  NA  NA  NA  NA
```
