# Why the market listings cannot be downloaded

Tells the visitors of the app when the API key has used up its credits
for the month or the day, and when the limit resets. For any other
failure, it says that the download is tried again in 10 minutes.

## Usage

``` r
listings_problem(info = key_info())
```

## Arguments

- info:

  The answer of
  [`key_info()`](https://sedzinfo.github.io/golemcrypto/reference/key_info.md).

## Value

A character string for the visitors of the app.

## Examples

``` r
info <- list(
  status = list(error_code = 0),
  data = list(
    plan = list(credit_limit_monthly_reset_timestamp = "2026-11-01T00:00:00.000Z"),
    usage = list(current_month = list(credits_left = 0))
  )
)
golemcrypto:::listings_problem(info)
#> [1] "The app has reached its monthly limit of CoinMarketCap requests, so the market data cannot be downloaded until the limit resets on 1 November 2026 at 00:00 UTC. Please come back then."
```
