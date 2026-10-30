##########################################################################################
#
##########################################################################################
#' Treemap module - UI
#'
#' The source and time of the last listings download with a button that
#' downloads the treemap's data as CSV, and a treemap of the top 100 coins.
#'
#' @param id Character. Module namespace id.
#'
#' @return A `tagList` of Shiny UI elements.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' shiny::fluidPage(golemcrypto:::mod_treemap_ui("treemap"))
#' }
mod_treemap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex align-items-center gap-3",
      helpText(textOutput(ns("updated"), inline = TRUE)),
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
#' Treemap module - server
#'
#' Renders the time of the last listings download and the treemap, and
#' serves the top 100 coins as a CSV file named like `treemap-2026-10-24.csv`,
#' with the columns `name_symbol`, `market_cap` and `change_24h`. When the
#' listings cannot be downloaded, it shows why in place of the treemap.
#'
#' @param id Character. Module namespace id, must match the id used in
#'   [mod_treemap_ui()].
#' @param listings Reactive returning the result of [get_listings()].
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
#'   mod_treemap_server("treemap", reactive(get_listings()), reactive(input$dark_mode))
#' }
#' }
mod_treemap_server <- function(id, listings, dark_mode) {
  moduleServer(id, function(input, output, session) {
    output$updated <- renderText(updated_text(listings()$time, listings()$problem))
    output$plot <- plotly::renderPlotly({
      validate(need(listings()$data, listings()$problem))
      plot_treemap(listings()$data, dark_mode())
    })
    output$csv <- csv_download(
      reactive({
        req(listings()$data)
        top <- utils::head(listings()$data, 100)
        data.frame(
          name_symbol = top$name_symbol,
          market_cap = top$USD_market_cap,
          change_24h = top$USD_percent_change_24h
        )
      }),
      "treemap"
    )
  })
}
