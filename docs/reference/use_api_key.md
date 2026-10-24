# Set up the 'CoinMarketCap' API key

Checks that there is a key and passes it to 'coinmarketcapr', which also
sets the address of the API.

## Usage

``` r
use_api_key(api_key = Sys.getenv("COINMARKETCAP_APIKEY"))
```

## Arguments

- api_key:

  A 'CoinMarketCap' API key, by default the `COINMARKETCAP_APIKEY`
  environment variable.

## Value

The key, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
crypto:::use_api_key()
} # }
```
