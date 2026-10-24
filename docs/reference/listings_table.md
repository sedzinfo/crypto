# Table of the market listings

A sortable table with 100 rows per page and numbers rounded to 2
decimals, showing the price in dollars and the price changes in percent.
Losses are shaded red and gains green, more strongly the larger the
change, in steps of 0.1 percentile from the 5th to the 95th. Changes
near zero keep the table's own background, so the table suits the light
and the dark mode.

## Usage

``` r
listings_table(listings)
```

## Arguments

- listings:

  The `data` of
  [`get_listings()`](https://sedzinfo.github.io/crypto/reference/get_listings.md).

## Value

A DT datatable.

## Examples

``` r
if (FALSE) { # \dontrun{
listings_table(get_listings()$data)
} # }
```
