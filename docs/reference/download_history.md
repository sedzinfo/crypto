# Download the daily price history of a coin

Downloads the daily USD prices with
[`crypto2::crypto_history()`](https://www.sebastianstoeckl.com/crypto2/dev/reference/crypto_history.html),
in requests of 400 days. A failed download is reported as a message
instead of an error.

## Usage

``` r
download_history(coin, start_date)
```

## Arguments

- coin:

  One row of
  [`get_coins()`](https://sedzinfo.github.io/crypto/reference/get_coins.md).

- start_date:

  Date. The first day to download.

## Value

The price history of the coin, as described in
[`get_history()`](https://sedzinfo.github.io/crypto/reference/get_history.md),
or `NULL` when nothing could be downloaded.

## Examples

``` r
if (FALSE) { # \dontrun{
coins <- get_coins()
crypto:::download_history(coins[coins$name_symbol == "Bitcoin (BTC)", ], Sys.Date() - 30)
} # }
```
