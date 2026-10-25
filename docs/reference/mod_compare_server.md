# Compare module - server

Downloads the price history of the selected coins from the start date,
renders their close prices normalized to 100, and serves them as a CSV
file named like `compare-2026-10-24.csv`, with the columns `date`,
`name_symbol`, `close` and `index`. The download waits until the
selection has not changed for 1.5 seconds, instead of starting on every
click in the picker.

## Usage

``` r
mod_compare_server(id, coins, dark_mode)
```

## Arguments

- id:

  Character. Module namespace id, must match the id used in
  [`mod_compare_ui()`](https://sedzinfo.github.io/golemcrypto/reference/mod_compare_ui.md).

- coins:

  Data frame of coins from
  [`get_coins()`](https://sedzinfo.github.io/golemcrypto/reference/get_coins.md).

- dark_mode:

  Reactive returning `"light"` or `"dark"`, the mode of the app.

## Value

None. Called for its side effect of registering the module server.

## Examples

``` r
if (FALSE) { # \dontrun{
# inside app_server():
function(input, output, session) {
  mod_compare_server("compare", coins, reactive(input$dark_mode))
}
} # }
```
