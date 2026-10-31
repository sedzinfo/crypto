# Crypto Time Series Analyzer

plot crypto exchange rate time series

An interactive R Shiny application for visualizing cryptocurrency exchange rates and market data with real-time updates and historical time series analysis.

## Features

- **Interactive Candlestick Charts**: Visualize cryptocurrency price movements with open, high, low, and close data
- **Real-Time Market Data**: Live prices and market statistics for the top 100 cryptocurrencies, refreshed every 10 minutes
- **Historical Analysis**: View historical price data dating back to 2013
- **Advanced Filtering**: Live search functionality to quickly find any cryptocurrency
- **Multiple Timeframes**: Flexible date range selectors (7 days to all-time) with custom range slider
- **Volume Analysis**: Dedicated volume charts to track trading activity
- **Moving Averages and Drawdown**: 50- and 200-day moving averages on the price chart, and a chart of how far the price is below its all-time high
- **Market Metrics**: Comprehensive market data including market cap, dominance, and 24h/7d/30d/60d/90d price changes
- **Color-Coded Performance**: Heat-map style coloring for quick visual identification of price movements
- **Coin Comparison**: Compare up to 10 coins, with their prices normalized to start at 100
- **Market Treemap**: The top 100 coins sized by market cap and colored by their 24h change
- **Returns Calendar**: Monthly returns by year for any coin
- **Light and Dark Mode**: Switch with the toggle in the navigation bar
- **CSV Downloads**: Download the data behind every tab as a CSV file
- **About Tab**: What every tab shows and how its numbers are calculated, with the formulas

# Quick Start

## Installation Instructions

### Prerequisites

- R (≥ 4.0.0) - [Download here](https://cran.r-project.org/)
- RStudio IDE (recommended) - [Download here](https://posit.co/downloads/)

### install R
installation instructions can be found here: https://cran.r-project.org/  
### install RStudio IDE (optional but a good idea)  
installation instructions can be found here: https://posit.co/downloads/  

Open RStudio (or an R console) and run:
```r
# Install devtools if you haven't already
install.packages("devtools")
library(devtools)
# Install the crypto package from GitHub
install_github("sedzinfo/golemcrypto")
```

## CoinMarketCap API key

The market data comes from the CoinMarketCap API, which needs a free API key.

1. Sign up at [coinmarketcap.com/api](https://coinmarketcap.com/api/) and copy your API key.
2. Open your `.Renviron` file:
   ```r
   usethis::edit_r_environ()
   ```
3. Add this line with your own key, save the file and restart R:
   ```
   COINMARKETCAP_APIKEY=your-api-key
   ```

You can also pass the key directly with `crypto(api_key = "your-api-key")`. Keep the key out of your code and out of Git, because anyone who has it can use your API credits.

Only the market listings of the Top 100 and Treemap tabs use the key: one credit per download, at most one download every 10 minutes while the app runs. When the key has used up its credits, the app still starts; those two tabs tell the visitors when the limit resets, and the other tabs work as usual.

## Usage
Launch the Shiny app with just two lines of code:

```r
library(golemcrypto)
crypto()
```

The application will open in your default web browser. Any [`shiny::shinyApp()`](https://shiny.posit.co/r/reference/shiny/latest/shinyapp.html) option can be passed on, for example `crypto(options = list(port = 8080))`.

## Using the Functions in R

The data, calculations and charts of the app are also available as functions, for scripts and R Markdown reports:

```r
library(golemcrypto)

# one coin since 2020: candlestick chart and monthly returns
btc <- get_history("Bitcoin (BTC)", as.Date("2020-01-01"))
plot_timeseries(btc)
plot_timeseries(btc, averages = c(20, 100))   # other moving averages
plot_returns_calendar(monthly_returns(btc), "Bitcoin (BTC)", mode = "dark")

# several coins over the last year, starting at 100
history <- get_history(c("Bitcoin (BTC)", "Ethereum (ETH)", "Solana (SOL)"))
plot_compare(normalize_prices(history))

# the top 100 coins, which needs the API key
listings <- get_listings()$data
plot_treemap(listings)
listings_table(listings)
```

Coins are picked by their label, such as `"Bitcoin (BTC)"`, and `get_coins()$name_symbol` lists all of them. Downloads are reused: the price histories and the list of coins are kept on disk between R sessions (see [Data Source](#data-source)), and the listings for 10 minutes.

## Application Overview

### Timeseries Tab
- Search and select any cryptocurrency using the live search box
- Candlestick chart showing price movements over time
- Interactive chart with zoom, pan, and hover tooltips
- Built-in date range selector with preset options
- Volume chart synchronized with the main price chart
- 50- and 200-day moving averages; click a legend entry to hide or show a line
- Drawdown chart: how far the price is below its all-time high

### Top 100 Tab
- Data table of the top 100 cryptocurrencies by market cap, refreshed every 10 minutes, with the time of the last update
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
- Each coin's close price starts at 100 on the first date all of them have data, so their performance can be compared directly
- Switch between a linear and a log scale

### Treemap Tab
- The top 100 coins, each box sized by market cap and colored from red to green by its 24h change

### Returns Calendar Tab
- Monthly returns by year for the coin selected in the Timeseries tab

### About Tab
- What every tab shows and how its numbers are calculated: the candles, moving averages and drawdown, the index of the Compare tab, the Top 100 columns, and the colors of the tables and charts
- Linear and log scales: how each is drawn, with an example chart, and when a log scale helps
- The text is in `inst/app/about.md`, so it can be edited without touching the R code

## Screenshots

### Cryptocurrency Price Chart
![Candlestick and volume chart of Bitcoin since 2013](https://github.com/sedzinfo/golemcrypto/raw/main/screenshots/crypto1.png)

### Top 100 Market Overview
![Table of the top 100 coins with color-coded price changes](https://github.com/sedzinfo/golemcrypto/raw/main/screenshots/crypto2.png)

### Coin Comparison
![Close prices of Bitcoin, Ethereum and Tether normalized to 100](https://github.com/sedzinfo/golemcrypto/raw/main/screenshots/crypto3.png)

### Market Treemap (dark mode)
![Treemap of the top 100 coins sized by market cap and colored by 24h change](https://github.com/sedzinfo/golemcrypto/raw/main/screenshots/crypto4.png)

### Returns Calendar (dark mode)
![Monthly returns of Bitcoin by year](https://github.com/sedzinfo/golemcrypto/raw/main/screenshots/crypto5.png)

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

Market data is provided by [CoinMarketCap](https://coinmarketcap.com/) through their API.

Price histories are downloaded with [crypto2](https://github.com/sstoeckl/crypto2) and kept on disk between R sessions, in the folder given by `tools::R_user_dir("crypto", "cache")`. CoinMarketCap sends at most 400 days per request, so the first download of a coin's full history takes about 13 requests; after that, only the days since the last download are fetched, in a single request. To keep the downloads somewhere else, set `options(crypto.cache_dir = "path/to/folder")`, and delete the folder to start over.

CoinMarketCap pauses clients that send too many requests in a short time. When that happens the R console shows `Retrying in 60 seconds` and the chart appears about a minute later.

## Development

The package follows the layout of a [golem](https://thinkr-open.github.io/golem/) app, without depending on golem:

| File | Contents |
|---|---|
| `R/run_app.R` | `crypto()`, which starts the app |
| `R/app_ui.R`, `R/app_server.R` | The page layout and the server, which connect the modules |
| `R/mod_*.R` | One Shiny module per tab |
| `R/fct_data.R` | Downloads, and the cache of the market listings |
| `R/fct_calc.R` | Calculations: normalized prices and monthly returns |
| `R/fct_plots.R` | The plotly charts and the Top 100 table |
| `R/utils_helpers.R` | Color scales and light/dark mode helpers |
| `inst/app/www/` | Favicon and the keep-alive script |
| `inst/app/about.md` | The text of the About tab |
| `tests/testthat/` | Tests, which run offline on small sample data |

Run the tests with `devtools::test()`, and the full package check with `devtools::check()`.

### Deploying to shinyapps.io

`app.R` starts the app on [shinyapps.io](https://www.shinyapps.io/), with Google Analytics behind the cookie banner of the sedzinfo.github.io sites, and without the keep-alive pings, so the app goes to sleep when nobody uses it instead of using up the active hours of the plan. `.rscignore` keeps the upload to the files the app needs.

1. Create a `.Renviron` file in the package folder with `COINMARKETCAP_APIKEY=your-api-key`. It is uploaded with the app but never committed, as `.gitignore` lists it.
2. Connect your account once, with the `rsconnect::setAccountInfo()` line from Account, Tokens on shinyapps.io.
3. Run `rsconnect::deployApp(appName = "golemcrypto")` from the package folder, or open `app.R` in RStudio and click Publish.

## Contributing

Contributions, issues, and feature requests are welcome! Feel free to check the [issues page](https://github.com/sedzinfo/golemcrypto/issues).

---

![Stars](https://img.shields.io/github/stars/sedzinfo/golemcrypto)
![Watchers](https://img.shields.io/github/watchers/sedzinfo/golemcrypto)
![Repo Size](https://img.shields.io/github/repo-size/sedzinfo/golemcrypto)
![Open Issues](https://img.shields.io/github/issues/sedzinfo/golemcrypto)
![Forks](https://img.shields.io/github/forks/sedzinfo/golemcrypto)
![Last Commit](https://img.shields.io/github/last-commit/sedzinfo/golemcrypto)
![Contributors](https://img.shields.io/github/contributors/sedzinfo/golemcrypto)
![License](https://img.shields.io/github/license/sedzinfo/golemcrypto)
![Workflow Status](https://img.shields.io/github/actions/workflow/status/sedzinfo/golemcrypto/build-tarball.yml)
![R-CMD-check](https://github.com/sedzinfo/golemcrypto/actions/workflows/R-CMD-check.yaml/badge.svg)
