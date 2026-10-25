# Top 100 module - server

Renders the time of the last listings download and the listings table,
and serves the listings as a CSV file named like
`top100-2026-10-24.csv`, with the column names of the table and the
values unrounded.

## Usage

``` r
mod_top100_server(id, listings)
```

## Arguments

- id:

  Character. Module namespace id, must match the id used in
  [`mod_top100_ui()`](https://sedzinfo.github.io/golemcrypto/reference/mod_top100_ui.md).

- listings:

  Reactive returning the result of
  [`get_listings()`](https://sedzinfo.github.io/golemcrypto/reference/get_listings.md).

## Value

None. Called for its side effect of registering the module server.

## Examples

``` r
if (FALSE) { # \dontrun{
# inside app_server():
function(input, output, session) {
  mod_top100_server("top100", reactive(get_listings()))
}
} # }
```
