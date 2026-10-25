# Launch the Crypto Time Series Analyzer

Starts an interactive 'shiny' application for exploring cryptocurrency
exchange rates and market data from 'CoinMarketCap'.

## Usage

``` r
crypto(
  api_key = Sys.getenv("COINMARKETCAP_APIKEY"),
  analytics = FALSE,
  keep_alive = TRUE,
  ...
)
```

## Arguments

- api_key:

  A 'CoinMarketCap' API key, needed for the market listings. By default
  it is read from the `COINMARKETCAP_APIKEY` environment variable, which
  can be set in `.Renviron`.

- analytics:

  `TRUE` to add Google Analytics, through the cookie banner shared with
  the sites of sedzinfo.github.io, which loads it only after the visitor
  accepts. Meant for the public deployment (see `app.R`); off by
  default, so that running the app yourself sends nothing.

- keep_alive:

  `TRUE` (the default) to ping the server every 15 seconds while the
  page is open, so that proxies do not close idle connections. `FALSE`
  lets the app go to sleep when nobody uses it, as `app.R` does on
  shinyapps.io, where an app that stays awake uses up the active hours
  of the plan.

- ...:

  Further arguments passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html),
  for example `options = list(port = 8080)`.

## Value

A [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object. Printing it, or calling `crypto()` at the console, opens the app
in the browser.

## Details

The application has the following tabs:

- Timeseries:

  Candlestick chart of daily open, high, low and close prices in USD
  since 2013 for any active cryptocurrency, with its 50- and 200-day
  moving averages, and charts of the volume and of the drawdown (how far
  the price is below its all-time high) underneath.

- Top 100:

  Table of the top 100 cryptocurrencies by market cap with price,
  volume, 24h to 90d price changes, market dominance and fully diluted
  market cap.

- Compare:

  Close prices of up to 10 coins, normalized to 100 on the first date
  all of them have data.

- Treemap:

  Top 100 coins sized by market cap and colored by 24h change.

- Returns calendar:

  Monthly returns by year for the coin selected in the Timeseries tab.

Market listings are downloaded when the app starts and refreshed every
10 minutes. Historical prices are downloaded on demand with
[`crypto2::crypto_history()`](https://www.sebastianstoeckl.com/crypto2/dev/reference/crypto_history.html)
and kept on disk between R sessions, so after the first download only
the days since the last one are fetched. An internet connection is
required. When 'CoinMarketCap' limits the requests, crypto2 prints
"Retrying in 60 seconds" and the chart appears about a minute later.

## Examples

``` r
if (interactive()) crypto()

# with the API key passed directly, on a fixed port
if (interactive()) crypto(api_key = "your-api-key", options = list(port = 8080))
```
