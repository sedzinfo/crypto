##########################################################################################
#
##########################################################################################
#' Top 100 module - UI
#'
#' The source and time of the last listings download with a button that
#' downloads the listings as CSV, and a sortable table of the top 100 coins.
#'
#' @param id Character. Module namespace id.
#'
#' @return A `tagList` of Shiny UI elements.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' shiny::fluidPage(golemcrypto:::mod_top100_ui("top100"))
#' }
mod_top100_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex align-items-center gap-3",
      helpText(textOutput(ns("updated"), inline = TRUE)),
      downloadButton(ns("csv"), "CSV", class = "btn-sm")
    ),
    DT::dataTableOutput(ns("table"))
  )
}
##########################################################################################
#
##########################################################################################
#' Top 100 module - server
#'
#' Renders the time of the last listings download and the listings table,
#' and serves the listings as a CSV file named like `top100-2026-10-24.csv`,
#' with the column names of the table and the values unrounded. When the
#' listings cannot be downloaded, it shows why in place of the table.
#'
#' @param id Character. Module namespace id, must match the id used in
#'   [mod_top100_ui()].
#' @param listings Reactive returning the result of [get_listings()].
#'
#' @return None. Called for its side effect of registering the module server.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # inside app_server():
#' function(input, output, session) {
#'   mod_top100_server("top100", reactive(get_listings()))
#' }
#' }
mod_top100_server <- function(id, listings) {
  moduleServer(id, function(input, output, session) {
    output$updated <- renderText(updated_text(listings()$time, listings()$problem))
    output$table <- DT::renderDataTable({
      validate(need(listings()$data, listings()$problem))
      listings_table(listings()$data)
    })
    output$csv <- csv_download(
      reactive({
        req(listings()$data)
        stats::setNames(listings()$data, gsub("USD_|percent_", "", names(listings()$data)))
      }),
      "top100"
    )
  })
}
