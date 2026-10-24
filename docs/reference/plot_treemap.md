# Treemap of the top 100 coins

Each box is sized by market cap and colored from red to green by the 24h
price change. The color limits are the 5th and 95th percentiles of the
changes, so a few outliers do not wash out the rest. Coins without a 24h
change are labelled n/a and colored white.

## Usage

``` r
plot_treemap(listings, mode = "light")
```

## Arguments

- listings:

  The `data` of
  [`get_listings()`](https://sedzinfo.github.io/crypto/reference/get_listings.md).

- mode:

  `"light"` (the default) or `"dark"`, to suit a light or dark page.

## Value

A plotly object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_treemap(get_listings()$data)
} # }
```
