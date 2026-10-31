##########################################################################################
#
##########################################################################################
#' @title The text of the About tab
#' @description Reads `inst/app/about.md` and fills in how often the data are
#' downloaded again, so that the text follows `refresh_minutes`,
#' `history_minutes` and `coins_minutes`.
#' @return The Markdown text, as one string.
#' @keywords internal
#' @examples
#' cat(substr(golemcrypto:::about_text(), 1, 300))
about_text <- function() {
  text <- paste(readLines(app_sys("app/about.md"), encoding = "UTF-8"), collapse = "\n")
  values <- c(refresh_minutes = refresh_minutes, history_minutes = history_minutes, coins_hours = coins_minutes / 60)
  for (name in names(values)) {
    text <- gsub(paste0("{{", name, "}}"), values[[name]], text, fixed = TRUE)
  }
  text
}
##########################################################################################
#
##########################################################################################
#' @title About module - UI
#' @description What each tab shows and how its numbers are calculated, from
#' [about_text()], with an example of the linear and log axes of the Compare
#' tab where the text marks its place.
#' @param id Character. Module namespace id.
#' @return A `div` of Shiny UI elements.
#' @keywords internal
#' @examples
#' \dontrun{
#' shiny::fluidPage(golemcrypto:::mod_about_ui("about"))
#' }
mod_about_ui <- function(id) {
  ns <- NS(id)
  text <- strsplit(about_text(), "<!-- scales example -->", fixed = TRUE)[[1]]
  div(
    class = "about",
    markdown(text[1]),
    plotly::plotlyOutput(ns("example"), width = "100%", height = "340px"),
    if (length(text) > 1) markdown(text[2])
  )
}
##########################################################################################
#
##########################################################################################
#' @title About module - server
#' @description Draws the example of the linear and log axes in the light or
#' dark mode of the app.
#' @param id Character. Module namespace id, must match the id used in
#'   [mod_about_ui()].
#' @param dark_mode Reactive returning `"light"` or `"dark"`, the mode of the
#'   app.
#' @return None. Called for its side effect of registering the module server.
#' @keywords internal
#' @examples
#' \dontrun{
#' # inside app_server():
#' function(input, output, session) {
#'   mod_about_server("about", reactive(input$dark_mode))
#' }
#' }
mod_about_server <- function(id, dark_mode) {
  moduleServer(id, function(input, output, session) {
    output$example <- plotly::renderPlotly(plot_scales(dark_mode()))
  })
}
