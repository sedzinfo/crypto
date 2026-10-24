# Treemap module - UI

The source and time of the last listings download with a button that
downloads the treemap's data as CSV, and a treemap of the top 100 coins.

## Usage

``` r
mod_treemap_ui(id)
```

## Arguments

- id:

  Character. Module namespace id.

## Value

A `tagList` of Shiny UI elements.

## Examples

``` r
if (FALSE) { # \dontrun{
shiny::fluidPage(crypto:::mod_treemap_ui("treemap"))
} # }
```
