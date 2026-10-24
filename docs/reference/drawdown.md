# Drawdown of prices

How far each close is below the highest close up to that day, the
all-time high so far, in percent: 0 on a new high and -50 at half of it.

## Usage

``` r
drawdown(close)
```

## Arguments

- close:

  Numeric vector of close prices, sorted by date.

## Value

A numeric vector as long as `close`, from -100 to 0.

## Examples

``` r
drawdown(c(100, 120, 90, 130, 65))
#> [1]   0   0 -25   0 -50
```
