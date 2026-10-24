# Keep a downloaded price history on disk

A history with missing days (see
[`has_gaps()`](https://sedzinfo.github.io/crypto/reference/has_gaps.md))
is returned but not kept, so the next call downloads it again.

## Usage

``` r
keep_history(key, history, from)
```

## Arguments

- key:

  Key of the coin in
  [`disk_cache()`](https://sedzinfo.github.io/crypto/reference/disk_cache.md).

- history:

  Price history of the coin, or `NULL` when the download failed.

- from:

  Date. The first day asked for, which can be earlier than the first day
  of the coin.

## Value

A list with the `history`, `from` and the `time` of the download, or
`NULL` when `history` is `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
coins <- get_coins()
btc <- coins[coins$name_symbol == "Bitcoin (BTC)", ]
crypto:::keep_history("history_1", crypto:::download_history(btc, Sys.Date() - 30), Sys.Date() - 30)
} # }
```
