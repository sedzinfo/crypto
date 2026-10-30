# Color of a price change

Losses are red and gains green, more opaque the larger the change, so
that changes near zero take the color of the page in light and dark
mode. At most 75% opaque, so the text on top stays readable in both
modes.

## Usage

``` r
change_color(strength, gain)
```

## Arguments

- strength:

  Numbers from 0, no change, to 1, the largest change.

- gain:

  `TRUE` for gains, `FALSE` for losses.

## Value

CSS `rgba()` colors, one per `strength`.

## Examples

``` r
golemcrypto:::change_color(c(0, 0.5, 1), gain = TRUE)
#> [1] "rgba(25,135,84,0.000)" "rgba(25,135,84,0.375)" "rgba(25,135,84,0.750)"
```
