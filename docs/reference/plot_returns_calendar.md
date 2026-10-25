# Heatmap of monthly returns by year

Months are colored from red to green by their return. The color limit is
the 95th percentile of the absolute returns, so a few extreme months do
not wash out the rest.

## Usage

``` r
plot_returns_calendar(calendar, label = NULL, mode = "light")
```

## Arguments

- calendar:

  Matrix from
  [`monthly_returns()`](https://sedzinfo.github.io/golemcrypto/reference/monthly_returns.md).

- label:

  Label of the coin for the title, or `NULL` for none.

- mode:

  `"light"` (the default) or `"dark"`, to suit a light or dark page.

## Value

A plotly object.

## Examples

``` r
if (FALSE) { # \dontrun{
btc <- get_history("Bitcoin (BTC)", as.Date("2013-04-29"))
plot_returns_calendar(monthly_returns(btc), "Bitcoin (BTC)")
} # }
```
