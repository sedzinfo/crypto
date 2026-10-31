##########################################################################################
# 
##########################################################################################
#' @title Path to a file installed with the package
#' @param ... Character vectors with the subdirectories and file name, as in
#'   [system.file()].
#' @return The full path, or `""` when the file does not exist.
#' @noRd
#' @examples
#' \dontrun{
#' app_sys("app/www")
#' }
app_sys <- function(...) {
  system.file(..., package = "golemcrypto")
}
