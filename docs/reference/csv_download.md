# Download handler that saves a data frame as CSV

Builds the `downloadHandler()` behind the CSV buttons of the app. The
file is named after `stem` and today's date, such as
`top100-2026-10-24.csv`, and written with
[`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html) without
row names. When `data` or `stem` stop with
[`shiny::req()`](https://rdrr.io/pkg/shiny/man/req.html) or
[`shiny::validate()`](https://rdrr.io/pkg/shiny/man/validate.html), the
download fails and the app keeps running.

## Usage

``` r
csv_download(data, stem)
```

## Arguments

- data:

  Function or reactive returning the data frame to save.

- stem:

  Start of the file name: a character string, or a function or reactive
  returning one.

## Value

A
[`shiny::downloadHandler()`](https://rdrr.io/pkg/shiny/man/downloadHandler.html),
to assign to an output.

## Examples

``` r
if (FALSE) { # \dontrun{
# inside a module server, with downloadButton(ns("csv"), "CSV") in its UI:
output$csv <- crypto:::csv_download(reactive(head(mtcars)), "cars")
} # }
```
