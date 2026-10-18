# Crypto Time Series Analyzer 📈

plot crypto exchange rate time series

An interactive R Shiny application for visualizing cryptocurrency
exchange rates and market data with real-time updates and historical
time series analysis.

## Features

- **Interactive Candlestick Charts**: Visualize cryptocurrency price
  movements with open, high, low, and close data
- **Real-Time Market Data**: Access live cryptocurrency prices and
  market statistics for the top 100 cryptocurrencies
- **Historical Analysis**: View historical price data dating back to
  2013
- **Advanced Filtering**: Live search functionality to quickly find any
  cryptocurrency
- **Multiple Timeframes**: Flexible date range selectors (1 day to
  all-time) with custom range slider
- **Volume Analysis**: Dedicated volume charts to track trading activity
- **Market Metrics**: Comprehensive market data including market cap,
  dominance, and 24h/7d/30d/60d/90d price changes
- **Color-Coded Performance**: Heat-map style coloring for quick visual
  identification of price movements

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

``` r

# Install devtools if you haven't already
install.packages("devtools")
library(devtools)
# Install the crypto package from GitHub
install_github("sedzinfo/crypto")
```

## Usage

Launch the Shiny app with just two lines of code:

``` r

library(crypto)
crypto()
```

The application will open in your default web browser.

## Application Overview

### Timeseries Tab

- Search and select any cryptocurrency using the live search box
- Candlestick chart showing price movements over time
- Interactive chart with zoom, pan, and hover tooltips
- Built-in date range selector with preset options
- Volume chart synchronized with the main price chart

### Top 100 Tab

- Real-time data table of the top 100 cryptocurrencies by market cap
- Sortable columns for easy comparison
- Color-coded performance indicators for quick insights
- Key metrics including:
  - Current price
  - 24h volume and volume change
  - Price changes across multiple timeframes (24h, 7d, 30d, 60d, 90d)
  - Market cap and market dominance
  - Fully diluted market cap

## Screenshots

### Cryptocurrency Price Chart

![Crypto Price
Analysis](https://github.com/sedzinfo/crypto/raw/main/screenshots/crypto1.png)

Crypto Price Analysis

### Top 100 Market Overview

![Market Data
Table](https://github.com/sedzinfo/crypto/raw/main/screenshots/crypto2.png)

Market Data Table

## Dependencies

This package relies on several R packages:

- `shiny` - Web application framework
- `plotly` - Interactive plotting
- `shinyWidgets` - Enhanced UI widgets
- `crypto2` - Cryptocurrency historical data
- `coinmarketcapr` - CoinMarketCap API integration

All dependencies will be automatically installed with the package.

## Data Source

Market data is provided by [CoinMarketCap](https://coinmarketcap.com/)
through their API.

## Contributing

Contributions, issues, and feature requests are welcome! Feel free to
check the [issues page](https://github.com/sedzinfo/crypto/issues).

------------------------------------------------------------------------

![Stars](https://img.shields.io/github/stars/sedzinfo/crypto)![Watchers](https://img.shields.io/github/watchers/sedzinfo/crypto)![Repo
Size](https://img.shields.io/github/repo-size/sedzinfo/crypto)![Open
Issues](https://img.shields.io/github/issues/sedzinfo/crypto)![Forks](https://img.shields.io/github/forks/sedzinfo/crypto)![Last
Commit](https://img.shields.io/github/last-commit/sedzinfo/crypto)![Contributors](https://img.shields.io/github/contributors/sedzinfo/crypto)![License](https://img.shields.io/github/license/sedzinfo/crypto)![Workflow
Status](https://img.shields.io/github/actions/workflow/status/sedzinfo/crypto/build-tarball.yml)
