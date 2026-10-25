# Treemap module - server

Renders the time of the last listings download and the treemap, and
serves the top 100 coins as a CSV file named like
`treemap-2026-10-24.csv`, with the columns `name_symbol`, `market_cap`
and `change_24h`.

## Usage

``` r
mod_treemap_server(id, listings, dark_mode)
```

## Arguments

- id:

  Character. Module namespace id, must match the id used in
  [`mod_treemap_ui()`](https://sedzinfo.github.io/golemcrypto/reference/mod_treemap_ui.md).

- listings:

  Reactive returning the result of
  [`get_listings()`](https://sedzinfo.github.io/golemcrypto/reference/get_listings.md).

- dark_mode:

  Reactive returning `"light"` or `"dark"`, the mode of the app.

## Value

None. Called for its side effect of registering the module server.

## Examples

``` r
if (FALSE) { # \dontrun{
# inside app_server():
function(input, output, session) {
  mod_treemap_server("treemap", reactive(get_listings()), reactive(input$dark_mode))
}
} # }
```
