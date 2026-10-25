##########################################################################################
#
##########################################################################################
#' Address of the cookie banner shared by the sites of sedzinfo.github.io
#'
#' @noRd
consent_script <- "https://sedzinfo.github.io/consent.js"
##########################################################################################
#
##########################################################################################
#' Google Analytics for the public deployment
#'
#' Loads the cookie banner shared with the sites of sedzinfo.github.io, which
#' loads Google Analytics only after the visitor accepts, and the script that
#' reports the tabs and the CSV downloads of the app as pages and downloads.
#' The stylesheet of the app gives the banner the navy of those sites,
#' because the orange primary color of the app is too pale behind its white
#' text.
#'
#' @return A `tagList` for the head of the page.
#'
#' @keywords internal
#'
#' @examples
#' crypto:::analytics_resources()
analytics_resources <- function() {
  tagList(
    tags$script(src = consent_script, defer = NA),
    tags$script(src = "www/analytics.js")
  )
}
##########################################################################################
#
##########################################################################################
#' Footer with the links of the cookie banner
#'
#' "Cookie settings" opens the banner again, so that visitors can change
#' their choice at any time, as the privacy note promises.
#'
#' @return A `div` for the bottom of the page.
#'
#' @keywords internal
#'
#' @examples
#' crypto:::analytics_footer()
analytics_footer <- function() {
  div(
    class = "text-center small py-3",
    tags$a(href = "#cookie-settings", "Cookie settings"),
    HTML(" &middot; "),
    tags$a(href = "https://sedzinfo.github.io/privacy.html", "Privacy")
  )
}
