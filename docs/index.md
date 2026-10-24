# Crypto Time Series Analyzer

plot crypto exchange rate time series

An interactive R Shiny application for visualizing cryptocurrency
exchange rates and market data with real-time updates and historical
time series analysis.

## Features

- **Interactive Candlestick Charts**: Visualize cryptocurrency price
  movements with open, high, low, and close data
- **Real-Time Market Data**: Live prices and market statistics for the
  top 100 cryptocurrencies, refreshed every 10 minutes
- **Historical Analysis**: View historical price data dating back to
  2013
- **Advanced Filtering**: Live search functionality to quickly find any
  cryptocurrency
- **Multiple Timeframes**: Flexible date range selectors (7 days to
  all-time) with custom range slider
- **Volume Analysis**: Dedicated volume charts to track trading activity
- **Moving Averages and Drawdown**: 50- and 200-day moving averages on
  the price chart, and a chart of how far the price is below its
  all-time high
- **Market Metrics**: Comprehensive market data including market cap,
  dominance, and 24h/7d/30d/60d/90d price changes
- **Color-Coded Performance**: Heat-map style coloring for quick visual
  identification of price movements
- **Coin Comparison**: Compare up to 10 coins, with their prices
  normalized to start at 100
- **Market Treemap**: The top 100 coins sized by market cap and colored
  by their 24h change
- **Returns Calendar**: Monthly returns by year for any coin
- **Light and Dark Mode**: Switch with the toggle in the navigation bar
- **CSV Downloads**: Download the data behind every tab as a CSV file

# Quick Start

## Installation Instructions

### Prerequisites

- R (≥ 4.0.0) - [Download here](https://cran.r-project.org/)
- RStudio IDE (recommended) - [Download
  here](https://posit.co/downloads/)

### install R

installation instructions can be found here:
<https://cran.r-project.org/>\
\### install RStudio IDE (optional but a good idea)\
installation instructions can be found here:
<https://posit.co/downloads/>

Open RStudio (or an R console) and run:

`# Install devtools if you haven't already`` `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"devtools"``)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`devtools`](https://devtools.r-lib.org/)`)`` ``# Install the crypto package from GitHub`` `[`install_github`](https://devtools.r-lib.org/reference/install-deprecated.html)`(``"sedzinfo/crypto"``)`

## CoinMarketCap API key

The market data comes from the CoinMarketCap API, which needs a free API
key.

1.  Sign up at [coinmarketcap.com/api](https://coinmarketcap.com/api/)
    and copy your API key.

2.  Open your `.Renviron` file:

    `usethis``::`[`edit_r_environ`](https://usethis.r-lib.org/reference/edit.html)`(``)`

3.  Add this line with your own key, save the file and restart R:

        COINMARKETCAP_APIKEY=your-api-key

You can also pass the key directly with
`crypto(api_key = "your-api-key")`. Keep the key out of your code and
out of Git, because anyone who has it can use your API credits.

## Usage

Launch the Shiny app with just two lines of code:

[`library`](https://rdrr.io/r/base/library.html)`(`[`crypto`](https://sedzinfo.github.io/crypto/)`)`` `[`crypto`](https://sedzinfo.github.io/crypto/reference/crypto.md)`(``)`

The application will open in your default web browser. Any
[`shiny::shinyApp()`](https://shiny.posit.co/r/reference/shiny/latest/shinyapp.html)
option can be passed on, for example
`crypto(options = list(port = 8080))`.

## Using the Functions in R

The data, calculations and charts of the app are also available as
functions, for scripts and R Markdown reports:

[`library`](https://rdrr.io/r/base/library.html)`(`[`crypto`](https://sedzinfo.github.io/crypto/)`)`` `` ``# one coin since 2020: candlestick chart and monthly returns`` ``btc`` ``<-`` `[`get_history`](https://sedzinfo.github.io/crypto/reference/get_history.md)`(``"Bitcoin (BTC)"``, `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2020-01-01"``)``)`` `[`plot_timeseries`](https://sedzinfo.github.io/crypto/reference/plot_timeseries.md)`(``btc``)`` `[`plot_timeseries`](https://sedzinfo.github.io/crypto/reference/plot_timeseries.md)`(``btc``, averages ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``20``, ``100``)``)`` ``# other moving averages`` `[`plot_returns_calendar`](https://sedzinfo.github.io/crypto/reference/plot_returns_calendar.md)`(`[`monthly_returns`](https://sedzinfo.github.io/crypto/reference/monthly_returns.md)`(``btc``)``, ``"Bitcoin (BTC)"``, mode ``=`` ``"dark"``)`` `` ``# several coins over the last year, starting at 100`` ``history`` ``<-`` `[`get_history`](https://sedzinfo.github.io/crypto/reference/get_history.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"Bitcoin (BTC)"``, ``"Ethereum (ETH)"``, ``"Solana (SOL)"``)``)`` `[`plot_compare`](https://sedzinfo.github.io/crypto/reference/plot_compare.md)`(`[`normalize_prices`](https://sedzinfo.github.io/crypto/reference/normalize_prices.md)`(``history``)``)`` `` ``# the top 100 coins, which needs the API key`` ``listings`` ``<-`` `[`get_listings`](https://sedzinfo.github.io/crypto/reference/get_listings.md)`(``)``$``data`` `[`plot_treemap`](https://sedzinfo.github.io/crypto/reference/plot_treemap.md)`(``listings``)`` `[`listings_table`](https://sedzinfo.github.io/crypto/reference/listings_table.md)`(``listings``)`

Coins are picked by their label, such as `"Bitcoin (BTC)"`, and
`get_coins()$name_symbol` lists all of them. Downloads are reused: the
price histories and the list of coins are kept on disk between R
sessions (see [Data Source](#data-source)), and the listings for 10
minutes.

## Application Overview

### Timeseries Tab

- Search and select any cryptocurrency using the live search box
- Candlestick chart showing price movements over time
- Interactive chart with zoom, pan, and hover tooltips
- Built-in date range selector with preset options
- Volume chart synchronized with the main price chart
- 50- and 200-day moving averages; click a legend entry to hide or show
  a line
- Drawdown chart: how far the price is below its all-time high

### Top 100 Tab

- Data table of the top 100 cryptocurrencies by market cap, refreshed
  every 10 minutes, with the time of the last update
- Sortable columns for easy comparison
- Color-coded performance indicators for quick insights
- Key metrics including:
  - Current price
  - 24h volume and volume change
  - Price changes across multiple timeframes (24h, 7d, 30d, 60d, 90d)
  - Market cap and market dominance
  - Fully diluted market cap

### Compare Tab

- Pick up to 10 coins and a start date
- Each coin’s close price starts at 100 on the first date all of them
  have data, so their performance can be compared directly
- Switch between a linear and a log scale

### Treemap Tab

- The top 100 coins, each box sized by market cap and colored from red
  to green by its 24h change

### Returns Calendar Tab

- Monthly returns by year for the coin selected in the Timeseries tab

## Screenshots

### Cryptocurrency Price Chart

![Candlestick and volume chart of Bitcoin since
2013](https://github.com/sedzinfo/crypto/raw/main/screenshots/crypto1.png)

Candlestick and volume chart of Bitcoin since 2013

### Top 100 Market Overview

![Table of the top 100 coins with color-coded price
changes](https://github.com/sedzinfo/crypto/raw/main/screenshots/crypto2.png)

Table of the top 100 coins with color-coded price changes

### Coin Comparison

![Close prices of Bitcoin, Ethereum and Tether normalized to
100](https://github.com/sedzinfo/crypto/raw/main/screenshots/crypto3.png)

Close prices of Bitcoin, Ethereum and Tether normalized to 100

### Market Treemap (dark mode)

![Treemap of the top 100 coins sized by market cap and colored by 24h
change](https://github.com/sedzinfo/crypto/raw/main/screenshots/crypto4.png)

Treemap of the top 100 coins sized by market cap and colored by 24h
change

### Returns Calendar (dark mode)

![Monthly returns of Bitcoin by
year](https://github.com/sedzinfo/crypto/raw/main/screenshots/crypto5.png)

Monthly returns of Bitcoin by year

## Dependencies

This package relies on several R packages:

- `shiny` - Web application framework
- `bslib` - Bootstrap 5 theme and the light/dark mode toggle
- `plotly` - Interactive plotting
- `DT` - Interactive tables
- `shinyWidgets` - Enhanced UI widgets
- `shinycssloaders` - Loading spinners
- `crypto2` - Cryptocurrency historical data
- `coinmarketcapr` - CoinMarketCap API integration

All dependencies will be automatically installed with the package.

## Data Source

Market data is provided by [CoinMarketCap](https://coinmarketcap.com/)
through their API.

Price histories are downloaded with
[crypto2](https://github.com/sstoeckl/crypto2) and kept on disk between
R sessions, in the folder given by
`tools::R_user_dir("crypto", "cache")`. CoinMarketCap sends at most 400
days per request, so the first download of a coin’s full history takes
about 13 requests; after that, only the days since the last download are
fetched, in a single request. To keep the downloads somewhere else, set
`options(crypto.cache_dir = "path/to/folder")`, and delete the folder to
start over.

CoinMarketCap pauses clients that send too many requests in a short
time. When that happens the R console shows `Retrying in 60 seconds` and
the chart appears about a minute later.

## Development

The package follows the layout of a
[golem](https://thinkr-open.github.io/golem/) app, without depending on
golem:

| File | Contents |
|----|----|
| `R/run_app.R` | [`crypto()`](https://sedzinfo.github.io/crypto/reference/crypto.md), which starts the app |
| `R/app_ui.R`, `R/app_server.R` | The page layout and the server, which connect the modules |
| `R/mod_*.R` | One Shiny module per tab |
| `R/fct_data.R` | Downloads, and the cache of the market listings |
| `R/fct_calc.R` | Calculations: normalized prices and monthly returns |
| `R/fct_plots.R` | The plotly charts and the Top 100 table |
| `R/utils_helpers.R` | Color scales and light/dark mode helpers |
| `inst/app/www/` | Favicon and the keep-alive script |
| `tests/testthat/` | Tests, which run offline on small sample data |

Run the tests with
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html),
and the full package check with
[`devtools::check()`](https://devtools.r-lib.org/reference/check.html).

## Contributing

Contributions, issues, and feature requests are welcome! Feel free to
check the [issues page](https://github.com/sedzinfo/crypto/issues).

------------------------------------------------------------------------

![Stars](https://img.shields.io/github/stars/sedzinfo/crypto)![Watchers](https://img.shields.io/github/watchers/sedzinfo/crypto)![Repo
Size](https://img.shields.io/github/repo-size/sedzinfo/crypto)![Open
Issues](https://img.shields.io/github/issues/sedzinfo/crypto)![Forks](https://img.shields.io/github/forks/sedzinfo/crypto)![Last
Commit](https://img.shields.io/github/last-commit/sedzinfo/crypto)![Contributors](https://img.shields.io/github/contributors/sedzinfo/crypto)![License](https://img.shields.io/github/license/sedzinfo/crypto)![Workflow
Status](https://img.shields.io/github/actions/workflow/status/sedzinfo/crypto/build-tarball.yml)![R-CMD-check](https://github.com/sedzinfo/crypto/actions/workflows/R-CMD-check.yaml/badge.svg)
