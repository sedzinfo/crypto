# Minutes since a time

Minutes since a time

## Usage

``` r
minutes_since(time)
```

## Arguments

- time:

  POSIXct.

## Value

The number of minutes from `time` until now.

## Examples

``` r
golemcrypto:::minutes_since(Sys.time() - 90)
#> [1] 1.5
```
