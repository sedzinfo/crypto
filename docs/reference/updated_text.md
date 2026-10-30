# Text with the source and time of the market listings

Text with the source and time of the market listings

## Usage

``` r
updated_text(time, problem = NULL)
```

## Arguments

- time:

  POSIXct. When the listings were downloaded, `NULL` when no download
  has worked yet.

- problem:

  Why the last download failed, from
  [`get_listings()`](https://sedzinfo.github.io/golemcrypto/reference/get_listings.md),
  or `NULL` when it worked.

## Value

A character string such as "Data: CoinMarketCap, updated 14:32",
followed by `problem`, or `NULL` when there is no `time`.

## Examples

``` r
golemcrypto:::updated_text(Sys.time())
#> [1] "Data: CoinMarketCap, updated 11:52"
golemcrypto:::updated_text(Sys.time(), "The market data cannot be downloaded right now.")
#> [1] "Data: CoinMarketCap, updated 11:52. The market data cannot be downloaded right now."
```
