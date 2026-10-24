# Moving average of prices

The simple moving average: each day's value is the mean of the last
`days` closes, that day included, so the first `days - 1` days have
none.

## Usage

``` r
moving_average(close, days)
```

## Arguments

- close:

  Numeric vector of close prices, sorted by date.

- days:

  Number of days to average, such as 50 or 200.

## Value

A numeric vector as long as `close`, `NA` for the first `days - 1` days,
and all `NA` when there are fewer than `days` closes, as for a coin
younger than the average.

## Examples

``` r
moving_average(c(1, 2, 3, 4, 5), days = 3)
#> [1] NA NA  2  3  4
```
