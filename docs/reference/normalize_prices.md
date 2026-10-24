# Normalize close prices to 100

Every coin starts at 100 on the first date on which all of them have
data, so their lines can be compared directly: 150 means the price rose
by half since that date.

## Usage

``` r
normalize_prices(history)
```

## Arguments

- history:

  Price history from
  [`get_history()`](https://sedzinfo.github.io/crypto/reference/get_history.md),
  with `name_symbol`, `timestamp` and `close` columns, sorted by coin
  and date.

## Value

`history` from the first common date on, with an `index` column.

## Examples

``` r
history <- data.frame(
  name_symbol = rep(c("Alpha (A)", "Beta (B)"), each = 3),
  timestamp = rep(as.Date("2024-01-01") + 0:2, 2),
  close = c(10, 11, 12, 200, 180, 220)
)
normalize_prices(history)
#>   name_symbol  timestamp close index
#> 1   Alpha (A) 2024-01-01    10   100
#> 2   Alpha (A) 2024-01-02    11   110
#> 3   Alpha (A) 2024-01-03    12   120
#> 4    Beta (B) 2024-01-01   200   100
#> 5    Beta (B) 2024-01-02   180    90
#> 6    Beta (B) 2024-01-03   220   110
```
