##########################################################################################
#
##########################################################################################
#' @title Compare module - UI
#' @description A picker for up to 10 coins, a start date, a button that
#' downloads the normalized prices as CSV, and a chart of the coins' close
#' prices normalized to 100.
#' @param id Character. Module namespace id.
#' @param coins Data frame of coins from [get_coins()], whose `name_symbol`
#'   labels fill the picker.
#' @param top_coins Coin labels sorted by market cap; the first three that
#'   are in `coins` are selected at start.
#' @return A `tagList` of Shiny UI elements.
#' @keywords internal
#' @examples
#' \dontrun{
#' coins <- golemcrypto:::get_coins()
#' shiny::fluidPage(golemcrypto:::mod_compare_ui(
#'                 "compare",
#'                  coins,
#'                  c("Bitcoin (BTC)", "Ethereum (ETH)")))
#' }
mod_compare_ui <- function(id, coins, top_coins) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex flex-wrap align-items-end gap-3",
      shinyWidgets::pickerInput(ns("coins"),
        label = "Coins",
        choices = unique(coins$name_symbol),
        selected = utils::head(intersect(top_coins, coins$name_symbol), 3),
        multiple = TRUE,
        options = list(`live-search` = TRUE, `max-options` = 10, style = "border bg-body text-body")
      ),
      dateInput(ns("start"), label = "Start date", value = Sys.Date() - 365, max = Sys.Date() - 1),
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
#' @title Compare module - server
#' @description Downloads the price history of the selected coins from the start
#' date, renders their close prices normalized to 100, and serves them as a CSV
#' file named like `compare-2026-10-24.csv`, with the columns `date`,
#' `name_symbol`, `close` and `index`. The download waits until the selection
#' has not changed for 1.5 seconds, instead of starting on every click in the
#' picker.
#' @param id Character. Module namespace id, must match the id used in
#'   [mod_compare_ui()].
#' @param coins Data frame of coins from [get_coins()].
#' @param dark_mode Reactive returning `"light"` or `"dark"`, the mode of the
#'   app.
#' @return None. Called for its side effect of registering the module server.
#' @keywords internal
#' @examples
#' \dontrun{
#' # inside app_server():
#' function(input, output, session) {
#'   mod_compare_server("compare", coins, reactive(input$dark_mode))
#' }
#' }
mod_compare_server <- function(id, coins, dark_mode) {
  moduleServer(id, function(input, output, session) {
    selected <- debounce(reactive(input$coins), 1500)
    history <- reactive({
      req(selected(), input$start)
      get_history(selected(), input$start, coins)
    })
    prices <- reactive({
      validate(need(history(), "Data not available for the selected coins"))
      normalize_prices(history())
    })

    output$plot <- plotly::renderPlotly(plot_compare(prices(), dark_mode()))

    output$csv <- csv_download(
      reactive(data.frame(
        date = prices()$timestamp,
        name_symbol = prices()$name_symbol,
        close = prices()$close,
        index = prices()$index
      )),
      "compare"
    )
  })
}
