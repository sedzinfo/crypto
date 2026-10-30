# Ask 'CoinMarketCap' about the API key

Calls the key/info endpoint, which uses no credits, for the plan of the
key from
[`use_api_key()`](https://sedzinfo.github.io/golemcrypto/reference/use_api_key.md)
and the credits it has used and has left.

## Usage

``` r
key_info()
```

## Value

The answer as a list with `status` and, for a valid key, `data`, or
`NULL` when 'CoinMarketCap' cannot be reached.

## Examples

``` r
if (FALSE) { # \dontrun{
golemcrypto:::key_info()$data$usage$current_month
} # }
```
