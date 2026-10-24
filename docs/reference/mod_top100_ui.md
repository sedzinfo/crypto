# Top 100 module - UI

The source and time of the last listings download with a button that
downloads the listings as CSV, and a sortable table of the top 100
coins.

## Usage

``` r
mod_top100_ui(id)
```

## Arguments

- id:

  Character. Module namespace id.

## Value

A `tagList` of Shiny UI elements.

## Examples

``` r
if (FALSE) { # \dontrun{
shiny::fluidPage(crypto:::mod_top100_ui("top100"))
} # }
```
