# Returns calendar module - server

Renders the monthly returns of the coin selected in the Timeseries tab,
reusing its price history instead of downloading it again, and serves
them as a CSV file named like
`bitcoin-btc-monthly-returns-2026-10-24.csv`, with a `year` column and
one column per month.

## Usage

``` r
mod_returns_calendar_server(id, history, dark_mode)
```

## Arguments

- id:

  Character. Module namespace id, must match the id used in
  [`mod_returns_calendar_ui()`](https://sedzinfo.github.io/crypto/reference/mod_returns_calendar_ui.md).

- history:

  Reactive returning the price history of the selected coin, from
  [`mod_timeseries_server()`](https://sedzinfo.github.io/crypto/reference/mod_timeseries_server.md).

- dark_mode:

  Reactive returning `"light"` or `"dark"`, the mode of the app.

## Value

None. Called for its side effect of registering the module server.

## Examples

``` r
if (FALSE) { # \dontrun{
# inside app_server():
function(input, output, session) {
  dark_mode <- reactive(input$dark_mode)
  history <- mod_timeseries_server("timeseries", coins, dark_mode)
  mod_returns_calendar_server("returns_calendar", history, dark_mode)
}
} # }
```
