##########################################################################################
# 
##########################################################################################
#' @title Color of a price change
#' @description Losses are red and gains green, more opaque the larger the
#' change, so that changes near zero take the color of the page in light and
#' dark mode. At most 75% opaque, so the text on top stays readable in both
#' modes.
#' @param strength Numbers from 0, no change, to 1, the largest change.
#' @param gain `TRUE` for gains, `FALSE` for losses.
#' @return CSS `rgba()` colors, one per `strength`.
#' @keywords internal
#' @examples
#' golemcrypto:::change_color(c(0, 0.5, 1), gain = TRUE)
change_color <- function(strength, gain) {
  rgb <- if (gain) "25,135,84" else "220,53,69"
  sprintf("rgba(%s,%.3f)", rgb, 0.75 * strength)
}
##########################################################################################
#
##########################################################################################
#' @title Color scale for price changes
#' @description The plotly version of `change_color()`: losses below the
#' middle, gains above it and no color in the middle.
#' @noRd
colorscale_change <- list(
  c(0, change_color(1, gain = FALSE)),
  c(0.5, change_color(0, gain = FALSE)),
  c(0.5, change_color(0, gain = TRUE)),
  c(1, change_color(1, gain = TRUE))
)
##########################################################################################
#
##########################################################################################
#' @title Text color of the light or dark mode
#' @param mode `"light"` or `"dark"`, the mode of the app.
#' @return A CSS color, the body text color of Bootstrap in that mode.
#' @keywords internal
#' @examples
#' golemcrypto:::text_color("dark")
text_color <- function(mode) {
  if (identical(mode, "dark")) "#dee2e6" else "#212529"
}
##########################################################################################
# 
##########################################################################################
#' @title Match a plotly chart to the light or dark mode of the app
#' @description plotly ignores the Bootstrap theme, so the backgrounds are
#' made transparent and the text and grid colors are switched to suit the mode,
#' on every axis of a chart with several panels.
#' @param p A plotly object.
#' @param mode `"light"` or `"dark"`, the value of the dark mode toggle.
#' @return The plotly object `p`, with its layout changed.
#' @keywords internal
#' @examples
#' p <- plotly::plot_ly(x = 1:3, y = c(2, 1, 3), type = "scatter", mode = "lines")
#' golemcrypto:::plot_theme(p, "dark")
plot_theme <- function(p, mode) {
  grid <- list(gridcolor = if (identical(mode, "dark")) "rgba(255,255,255,0.1)" else "rgba(0,0,0,0.1)")
  # a chart made with subplot() has an axis per panel: yaxis2, yaxis3, ...
  axes <- union(c("xaxis", "yaxis"), grep("^[xy]axis[0-9]*$", names(p$x$layout), value = TRUE))
  do.call(plotly::layout, c(
    list(p,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = text_color(mode))
    ),
    stats::setNames(rep(list(grid), length(axes)), axes)
  ))
}
##########################################################################################
# 
##########################################################################################
#' @title Background colors of plotly buttons for the light or dark mode
#' @description plotly buttons default to a light background, which hides the
#' light text of the dark mode.
#' @param mode `"light"` or `"dark"`, the mode of the app.
#' @return A list with the `bgcolor` and `activecolor` of the buttons, as
#'   used by a plotly `rangeselector`.
#' @keywords internal
#' @examples
#' golemcrypto:::button_colors("dark")
button_colors <- function(mode) {
  if (identical(mode, "dark")) {
    list(bgcolor = "#343a40", activecolor = "#6c757d")
  } else {
    list(bgcolor = "#eeeeee", activecolor = "#d4d4d4")
  }
}
##########################################################################################
# 
##########################################################################################
#' @title Stop when a price history holds more than one coin
#' @description For the functions that work on one coin at a time, so that a
#' history of several coins gives a clear error instead of a wrong result.
#' @param history Price history from [get_history()].
#' @return `history`, invisibly, when it holds at most one coin.
#' @keywords internal
#' @examples
#' history <- data.frame(name_symbol = c("Alpha (A)", "Beta (B)"))
#' try(golemcrypto:::check_one_coin(history))
check_one_coin <- function(history) {
  coins <- unique(history$name_symbol)
  if (length(coins) > 1) {
    stop(
      "history holds ", length(coins), " coins, keep one of them first, for example ",
      "history[history$name_symbol == \"", coins[1], "\", ]",
      call. = FALSE
    )
  }
  invisible(history)
}
##########################################################################################
# 
##########################################################################################
#' @title Minutes since a time
#' @param time POSIXct.
#' @return The number of minutes from `time` until now.
#' @keywords internal
#' @examples
#' golemcrypto:::minutes_since(Sys.time() - 90)
minutes_since <- function(time) {
  as.numeric(difftime(Sys.time(), time, units = "mins"))
}
##########################################################################################
# 
##########################################################################################
#' @title Text with the source and time of the market listings
#' @param time POSIXct. When the listings were downloaded, `NULL` when no
#'   download has worked yet.
#' @param problem Why the last download failed, from [get_listings()], or
#'   `NULL` when it worked.
#' @return A character string such as "Data: CoinMarketCap, updated 14:32",
#'   followed by `problem`, or `NULL` when there is no `time`.
#' @keywords internal
#' @examples
#' golemcrypto:::updated_text(Sys.time())
#' golemcrypto:::updated_text(Sys.time(), "The market data cannot be downloaded right now.")
updated_text <- function(time, problem = NULL) {
  if (is.null(time)) {
    return(NULL)
  }
  paste0("Data: CoinMarketCap, updated ", format(time, "%H:%M"), if (!is.null(problem)) paste(".", problem))
}
