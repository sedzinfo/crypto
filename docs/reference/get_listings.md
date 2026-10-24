# Get the latest market listings

Returns the listings of the coins by market cap, with their price,
volume, price changes from 24 hours to 90 days, market cap and dominance
in USD. The listings are downloaded again when they are older than 10
minutes, and the download is shared by all sessions and app runs in the
same R process, so the API is called at most once in that time.

## Usage

``` r
get_listings()
```

## Value

A list with `data`, a data frame with one row per coin sorted by market
cap, and `time`, when it was downloaded.

## Details

The listings need a 'CoinMarketCap' API key in the
`COINMARKETCAP_APIKEY` environment variable, see the README. The first
download lets errors through. When a later download fails, the previous
listings and their time are kept, so the time stays truthful.

## Examples

``` r
if (FALSE) { # \dontrun{
listings <- get_listings()
listings$time
head(listings$data)
} # }
```
