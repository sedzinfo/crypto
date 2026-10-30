# Google Analytics for the public deployment

Loads the cookie banner shared with the sites of sedzinfo.github.io,
which loads Google Analytics only after the visitor accepts, and the
script that reports the tabs and the CSV downloads of the app as pages
and downloads. The stylesheet of the app gives the banner the navy of
those sites, because the orange primary color of the app is too pale
behind its white text.

## Usage

``` r
analytics_resources()
```

## Value

A `tagList` for the head of the page.

## Examples

``` r
golemcrypto:::analytics_resources()
#> <script src="https://sedzinfo.github.io/consent.js" defer></script>
#> <script src="www/analytics.js"></script>
```
