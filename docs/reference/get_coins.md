# List the active coins

Downloads the list of the active coins tracked by 'CoinMarketCap' and
keeps it on disk for a day (see
[`disk_cache()`](https://sedzinfo.github.io/crypto/reference/disk_cache.md)).
When the download fails, the kept list is used. Its `name_symbol` column
holds the labels, such as "Bitcoin (BTC)", that pick coins in
[`get_history()`](https://sedzinfo.github.io/crypto/reference/get_history.md).

## Usage

``` r
get_coins()
```

## Value

The
[`crypto2::crypto_list()`](https://www.sebastianstoeckl.com/crypto2/dev/reference/crypto_list.html)
data frame, with a `name_symbol` column.

## Examples

``` r
if (FALSE) { # \dontrun{
coins <- get_coins()
head(coins$name_symbol)
} # }
```
