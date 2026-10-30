# Get the price history of one coin, kept on disk and brought up to date

The first call downloads the history from `start_date` and keeps it on
disk (see
[`disk_cache()`](https://sedzinfo.github.io/golemcrypto/reference/disk_cache.md)).
Later calls use the kept history, and once it is more than an hour old
download only the days since its last day, because past days do not
change: one request instead of one per 400 days. A history kept from a
later day than `start_date` is downloaded again in full, and when
bringing it up to date fails, the kept history is used.

## Usage

``` r
coin_history(coin, start_date)
```

## Arguments

- coin:

  One row of
  [`get_coins()`](https://sedzinfo.github.io/golemcrypto/reference/get_coins.md).

- start_date:

  Date. The first day needed.

## Value

The price history of the coin from `start_date`, or `NULL` when it could
not be downloaded.

## Examples

``` r
if (FALSE) { # \dontrun{
coins <- get_coins()
golemcrypto:::coin_history(coins[coins$name_symbol == "Bitcoin (BTC)", ], Sys.Date() - 30)
} # }
```
