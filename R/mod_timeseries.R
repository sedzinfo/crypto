##########################################################################################
#
##########################################################################################
#' @title Timeseries module - UI
#' @description A coin picker with live search, a button that downloads the
#' coin's daily prices as CSV, and a candlestick chart of those prices, with the
#' trading volume underneath.
#' @param id Character. Module namespace id.
#' @param coins Data frame of coins from [get_coins()], whose `name_symbol`
#'   labels fill the picker.
#' @return A `tagList` of Shiny UI elements.
#' @keywords internal
#' @examples
#' \dontrun{
#' shiny::fluidPage(golemcrypto:::mod_timeseries_ui("timeseries", golemcrypto:::get_coins()))
#' }
mod_timeseries_ui <- function(id, coins) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex flex-wrap align-items-end gap-3",
      shinyWidgets::pickerInput(ns("coin"),
        label = "Live search",
        choices = unique(coins$name_symbol),
        # the page's own colors follow the light/dark mode, the default light button stays white
        options = list(`live-search` = TRUE, style = "border bg-body text-body")
      ),
      div(class = "mb-3", downloadButton(ns("csv"), "CSV"))
    ),
    shinycssloaders::withSpinner(
      # the rest of the window below the controls, with a minimum in styles.css
      plotly::plotlyOutput(ns("plot"), width = "100%", height = "calc(100vh - 170px)")
    )
  )
}
##########################################################################################
#
##########################################################################################
#' @title Timeseries module - server
#' @description Downloads the full price history of the selected coin, renders
#' its candlestick chart and serves it as a CSV file named like
#' `bitcoin-btc-history-2026-10-24.csv`, with the columns `date`, `name_symbol`,
#' `open`, `high`, `low`, `close`, `volume` and `market_cap`.
#' @param id Character. Module namespace id, must match the id used in
#'   [mod_timeseries_ui()].
#' @param coins Data frame of coins from [get_coins()].
#' @param dark_mode Reactive returning `"light"` or `"dark"`, the mode of the
#'   app.
#' @return A reactive returning the price history of the selected coin, from
#'   [get_history()], which the Returns calendar reuses.
#' @keywords internal
#' @examples
#' \dontrun{
#' # inside app_server():
#' function(input, output, session) {
#'   history <- mod_timeseries_server("timeseries", coins, reactive(input$dark_mode))
#' }
#' }
mod_timeseries_server <- function(id, coins, dark_mode) {
  moduleServer(id, function(input, output, session) {
    # downloads live in reactives, so toggling dark mode re-draws without re-downloading;
    # CoinMarketCap's history starts on 2013-04-29, an earlier date only adds a warning
    history <- reactive(get_history(input$coin, as.Date("2013-04-29"), coins))

    output$plot <- plotly::renderPlotly({
      validate(need(history(), "Data not available for this coin"))
      plot_timeseries(history(), dark_mode())
    })

    output$csv <- csv_download(
      reactive({
        req(history())
        columns <- intersect(c("name_symbol", "open", "high", "low", "close", "volume", "market_cap"), names(history()))
        data.frame(date = history()$timestamp, history()[columns])
      }),
      reactive({
        req(history())
        paste0(file_stem(unique(history()$name_symbol)), "-history")
      })
    )

    history
  })
}
