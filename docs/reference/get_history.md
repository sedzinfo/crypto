# Get the daily price history of coins

Returns the daily USD prices of one or more coins from 'CoinMarketCap'.
Each coin's history is kept on disk between R sessions, and once it is
more than an hour old only the days since its last day are downloaded,
so calls are fast and stay below the request limit of 'CoinMarketCap'.
When that limit is hit, crypto2 waits a minute and tries again.

## Usage

``` r
get_history(labels, start_date = Sys.Date() - 365, coins = get_coins())
```

## Arguments

- labels:

  Character vector of coin labels, such as "Bitcoin (BTC)", from the
  `name_symbol` column of
  [`get_coins()`](https://sedzinfo.github.io/golemcrypto/reference/get_coins.md).

- start_date:

  Date. The first day to get, a year ago by default.

- coins:

  Data frame of coins from
  [`get_coins()`](https://sedzinfo.github.io/golemcrypto/reference/get_coins.md),
  to look the labels up.

## Value

A data frame with one row per coin and day, sorted by coin and date,
with the `open`, `high`, `low`, `close` and `volume` columns,
`timestamp` as a Date and the `name_symbol` label. Labels that match no
coin are left out with a message, and `NULL` is returned when no label
matches or nothing could be downloaded.

## Examples

``` r
if (FALSE) { # \dontrun{
btc <- get_history("Bitcoin (BTC)", as.Date("2020-01-01"))
plot_timeseries(btc)

history <- get_history(c("Bitcoin (BTC)", "Ethereum (ETH)"))
plot_compare(normalize_prices(history))
} # }
```
