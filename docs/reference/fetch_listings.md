# Download the latest market listings

Calls the 'CoinMarketCap' API with the key from
[`use_api_key()`](https://sedzinfo.github.io/crypto/reference/use_api_key.md).

## Usage

``` r
fetch_listings()
```

## Value

A data frame with one row per coin, sorted by market cap, with a
`name_symbol` label and the USD price, volume, price change, market cap
and dominance columns shown in the app.

## Examples

``` r
if (FALSE) { # \dontrun{
listings <- crypto:::fetch_listings()
head(listings)
} # }
```
