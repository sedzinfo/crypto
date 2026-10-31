##########################################################################################
# 
##########################################################################################
#' @title The application server logic
#' @description Wires up the module of each tab. The market listings are
#' refreshed every 10 minutes and shared by the Top 100 and Treemap tabs, and
#' the price history of the coin picked in the Timeseries tab is shared with
#' the Returns calendar.
#' @param coins Data frame of coins from `get_coins()`.
#' @return A shiny server function, with the usual `input`, `output` and
#'   `session` arguments.
#' @noRd
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' shiny::shinyApp(ui = app_ui(coins, get_listings()$data$name_symbol),
#'                 server = app_server(coins))
#' }
app_server <- function(coins) {
  function(input, output, session) {
    dark_mode <- reactive(input$dark_mode)
    listings <- reactive({
      invalidateLater(refresh_minutes * 60 * 1000)
      get_listings()
    })

    history <- mod_timeseries_server("timeseries", coins, dark_mode)
    mod_top100_server("top100", listings)
    mod_compare_server("compare", coins, dark_mode)
    mod_treemap_server("treemap", listings, dark_mode)
    mod_returns_calendar_server("returns_calendar", history, dark_mode)

    output$keep_alive <- renderText({
      req(input$count)
      paste("keep alive", input$count)
    })
  }
}
