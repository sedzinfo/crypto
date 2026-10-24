# Background colors of plotly buttons for the light or dark mode

plotly buttons default to a light background, which hides the light text
of the dark mode.

## Usage

``` r
button_colors(mode)
```

## Arguments

- mode:

  `"light"` or `"dark"`, the mode of the app.

## Value

A list with the `bgcolor` and `activecolor` of the buttons, as used by a
plotly `rangeselector`.

## Examples

``` r
crypto:::button_colors("dark")
#> $bgcolor
#> [1] "#343a40"
#> 
#> $activecolor
#> [1] "#6c757d"
#> 
```
