##########################################################################################
#
##########################################################################################
#' @title Turn a label into a file name stem
#' @description Lower case, with every run of characters other than letters and
#' digits replaced by a hyphen, so that "Bitcoin (BTC)" becomes "bitcoin-btc".
#' @param label Character string, such as a coin label.
#' @return A character string that can be used in a file name.
#' @keywords internal
#' @examples
#' golemcrypto:::file_stem("Bitcoin (BTC)")
file_stem <- function(label) {
  gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(label)))
}
##########################################################################################
#
##########################################################################################
#' @title Download handler that saves a data frame as CSV
#' @description Builds the `downloadHandler()` behind the CSV buttons of the
#' app. The file is named after `stem` and today's date, such as
#' `top100-2026-10-24.csv`, and written with [utils::write.csv()] without row
#' names. When `data` or `stem` stop with [shiny::req()] or [shiny::validate()],
#' the download fails and the app keeps running.
#' @param data Function or reactive returning the data frame to save.
#' @param stem Start of the file name: a character string, or a function or
#'   reactive returning one.
#' @return A [shiny::downloadHandler()], to assign to an output.
#' @keywords internal
#' @examples
#' \dontrun{
#' # inside a module server, with downloadButton(ns("csv"), "CSV") in its UI:
#' output$csv <- golemcrypto:::csv_download(reactive(head(mtcars)), "cars")
#' }
csv_download <- function(data, stem) {
  downloadHandler(
    filename = function() {
      paste0(if (is.function(stem)) stem() else stem, "-", format(Sys.Date()), ".csv")
    },
    content = function(file) {
      utils::write.csv(data(), file, row.names = FALSE)
    }
  )
}
