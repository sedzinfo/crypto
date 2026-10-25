##########################################################################################
#
##########################################################################################
#' Returns calendar module - UI
#'
#' A button that downloads the monthly returns as CSV, and a heatmap of the
#' monthly returns of the coin selected in the Timeseries tab.
#'
#' @param id Character. Module namespace id.
#'
#' @return A `tagList` of Shiny UI elements.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' shiny::fluidPage(crypto:::mod_returns_calendar_ui("returns_calendar"))
#' }
mod_returns_calendar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex align-items-center gap-3",
      helpText("Coin selected in the Timeseries tab"),
      downloadButton(ns("csv"), "CSV", class = "btn-sm")
    ),
    shinycssloaders::withSpinner(
      # the rest of the window below the controls, with a minimum in styles.css
      plotly::plotlyOutput(ns("plot"), width = "100%", height = "calc(100vh - 130px)")
    )
  )
}
##########################################################################################
#
##########################################################################################
#' Returns calendar module - server
#'
#' Renders the monthly returns of the coin selected in the Timeseries tab,
#' reusing its price history instead of downloading it again, and serves
#' them as a CSV file named like `bitcoin-btc-monthly-returns-2026-10-24.csv`,
#' with a `year` column and one column per month.
#'
#' @param id Character. Module namespace id, must match the id used in
#'   [mod_returns_calendar_ui()].
#' @param history Reactive returning the price history of the selected coin,
#'   from [mod_timeseries_server()].
#' @param dark_mode Reactive returning `"light"` or `"dark"`, the mode of the
#'   app.
#'
#' @return None. Called for its side effect of registering the module server.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # inside app_server():
#' function(input, output, session) {
#'   dark_mode <- reactive(input$dark_mode)
#'   history <- mod_timeseries_server("timeseries", coins, dark_mode)
#'   mod_returns_calendar_server("returns_calendar", history, dark_mode)
#' }
#' }
mod_returns_calendar_server <- function(id, history, dark_mode) {
  moduleServer(id, function(input, output, session) {
    calendar <- reactive({
      validate(need(history(), "Data not available for this coin"))
      monthly_returns(history())
    })

    output$plot <- plotly::renderPlotly({
      validate(need(any(!is.na(calendar())), "Not enough history for monthly returns (needs at least two months)"))
      plot_returns_calendar(calendar(), unique(history()$name_symbol), dark_mode())
    })

    output$csv <- csv_download(
      reactive(data.frame(year = as.integer(rownames(calendar())), calendar(), check.names = FALSE, row.names = NULL)),
      reactive(paste0(file_stem(unique(history()$name_symbol)), "-monthly-returns"))
    )
  })
}
