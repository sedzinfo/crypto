# Text with the source and time of the market listings

Text with the source and time of the market listings

## Usage

``` r
updated_text(time)
```

## Arguments

- time:

  POSIXct. When the listings were downloaded.

## Value

A character string such as "Data: CoinMarketCap, updated 14:32".

## Examples

``` r
crypto:::updated_text(Sys.time())
#> [1] "Data: CoinMarketCap, updated 12:39"
```
