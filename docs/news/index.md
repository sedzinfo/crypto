# Changelog

## crypto 0.1.0

CRAN release: 2018-01-14

### Breaking changes

- [`crypto()`](https://sedzinfo.github.io/crypto/reference/crypto.md)
  needs your own CoinMarketCap API key, set in the
  `COINMARKETCAP_APIKEY` environment variable (for example in
  `.Renviron`) or passed as `crypto(api_key = )`. The package no longer
  comes with a key. See the README for how to get a free one.
- [`library(crypto)`](https://sedzinfo.github.io/crypto/) no longer
  attaches shiny, plotly, crypto2, coinmarketcapr and shinyWidgets. Load
  them yourself if your code uses their functions.

### New features

- Three new tabs:
  - **Compare** plots up to 10 coins from a chosen start date, each
    starting at 100, with a linear or log scale.
  - **Treemap** shows the top 100 coins sized by market cap and colored
    by their 24h change.
  - **Returns calendar** shows the monthly returns, by year, of the coin
    selected in the Timeseries tab.
- The Timeseries chart shows the 50- and 200-day moving averages, and a
  drawdown chart of how far the price is below its all-time high.
  [`moving_average()`](https://sedzinfo.github.io/crypto/reference/moving_average.md)
  and
  [`drawdown()`](https://sedzinfo.github.io/crypto/reference/drawdown.md)
  compute them, and
  [`plot_timeseries()`](https://sedzinfo.github.io/crypto/reference/plot_timeseries.md)
  gained the `averages` and `show_drawdown` arguments.
- The app has a name, a Bootstrap 5 theme, a favicon and a light/dark
  mode toggle. The charts and the Top 100 table follow the mode: price
  changes are shaded in a red and a green that fade into the page, so
  values near zero no longer show as white cells in dark mode.
- Every tab has a CSV button that downloads the data behind it: the
  coin’s daily prices, the listings, the normalized prices, the treemap
  data and the monthly returns.
- Charts show a loading spinner while their data downloads.
- The Top 100 listings refresh every 10 minutes and show when they were
  last updated.
- The data, calculations and charts of the app can be used directly from
  R:
  [`get_coins()`](https://sedzinfo.github.io/crypto/reference/get_coins.md),
  [`get_history()`](https://sedzinfo.github.io/crypto/reference/get_history.md),
  [`get_listings()`](https://sedzinfo.github.io/crypto/reference/get_listings.md),
  [`normalize_prices()`](https://sedzinfo.github.io/crypto/reference/normalize_prices.md),
  [`monthly_returns()`](https://sedzinfo.github.io/crypto/reference/monthly_returns.md),
  [`plot_timeseries()`](https://sedzinfo.github.io/crypto/reference/plot_timeseries.md),
  [`plot_compare()`](https://sedzinfo.github.io/crypto/reference/plot_compare.md),
  [`plot_returns_calendar()`](https://sedzinfo.github.io/crypto/reference/plot_returns_calendar.md),
  [`plot_treemap()`](https://sedzinfo.github.io/crypto/reference/plot_treemap.md)
  and
  [`listings_table()`](https://sedzinfo.github.io/crypto/reference/listings_table.md).
- Price histories and the list of coins are kept on disk between R
  sessions. After the first download of a coin, only the days since the
  last download are fetched, so charts appear faster and the app makes
  far fewer requests to CoinMarketCap. Set
  `options(crypto.cache_dir = )` to keep the downloads in another
  folder.
- `crypto(analytics = TRUE)` adds Google Analytics, through the cookie
  banner of the sedzinfo.github.io sites, which loads it only after the
  visitor accepts. The tabs and the CSV downloads are reported as pages
  and downloads. It is off by default and on in `app.R`, the file used
  to deploy the app on shinyapps.io.
- [`crypto()`](https://sedzinfo.github.io/crypto/reference/crypto.md)
  passes further arguments on to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html),
  for example `crypto(options = list(port = 8080))`.

### Bug fixes and smaller changes

- A failed download shows a short message, such as “Data not available
  for this coin”, instead of an R error.
- The keep-alive pings now get an answer from the server, so idle
  connections stay open behind proxies. `crypto(keep_alive = FALSE)`
  turns them off, as `app.R` does, so that the app can go to sleep on
  shinyapps.io when nobody uses it.
- Small prices are no longer shown in scientific notation, and the app
  no longer changes the options of your R session to do so.
- Treemap labels show the 24h change instead of NaN.
- The date range buttons of the Timeseries chart are readable in dark
  mode.
- Bitcoin’s full history no longer triggers a warning about dates before
  2013-04-29.
- A download that misses days, which can happen when CoinMarketCap
  limits requests, is not kept, so the next request downloads it again.

### Internal

- The code follows a golem-style layout, with one Shiny module per tab.
- Every function has a help page.
- Tests run offline, and R CMD check runs on GitHub Actions for every
  push.
