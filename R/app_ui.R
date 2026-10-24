##########################################################################################
# 
##########################################################################################
#' The application user interface
#'
#' A navbar page with one tab per module, a light/dark mode toggle and a
#' hidden output that answers the keep-alive pings.
#'
#' @param coins Data frame of coins from `get_coins()`, for the coin pickers.
#' @param top_coins Coin labels sorted by market cap, for the default
#'   selection of the Compare tab.
#'
#' @return A `tagList` with the whole page.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' shiny::shinyApp(ui = app_ui(coins, get_listings()$data$name_symbol), server = app_server(coins))
#' }
app_ui <- function(coins, top_coins) {
  app_name <- "Crypto Time Series Analyzer"
  tagList(
    add_external_resources(),
    bslib::page_navbar(
      title = app_name,
      window_title = app_name,
      theme = bslib::bs_theme(version = 5, primary = "#F7931A"),
      # keep the fixed plot heights instead of squeezing the plots to the window
      fillable = FALSE,
      tabPanel("Timeseries", mod_timeseries_ui("timeseries", coins)),
      tabPanel("Top 100", mod_top100_ui("top100")),
      tabPanel("Compare", mod_compare_ui("compare", coins, top_coins)),
      tabPanel("Treemap", mod_treemap_ui("treemap")),
      tabPanel("Returns calendar", mod_returns_calendar_ui("returns_calendar")),
      bslib::nav_spacer(),
      bslib::nav_item(bslib::input_dark_mode(id = "dark_mode"))
    ),
    # rendering this output makes the server answer the keep-alive pings
    div(style = "display:none", textOutput("keep_alive"))
  )
}
##########################################################################################
# 
##########################################################################################
#' Add external resources to the application
#'
#' Serves `inst/app/www` under the `www/` path and adds the favicon and the
#' keep-alive script to the head of the page.
#'
#' @return A `tags$head()` tag.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' # called from inside app_ui():
#' shiny::tagList(add_external_resources(), bslib::page_navbar())
#' }
add_external_resources <- function() {
  addResourcePath("www", app_sys("app/www"))
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
    tags$script(src = "www/keep_alive.js")
  )
}
