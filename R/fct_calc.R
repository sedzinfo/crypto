##########################################################################################
# 
##########################################################################################
#' Normalize close prices to 100
#'
#' Every coin starts at 100 on the first date on which all of them have data,
#' so their lines can be compared directly: 150 means the price rose by half
#' since that date.
#'
#' @param history Price history from [get_history()], with `name_symbol`,
#'   `timestamp` and `close` columns, sorted by coin and date.
#'
#' @return `history` from the first common date on, with an `index` column.
#'
#' @export
#'
#' @examples
#' history <- data.frame(
#'   name_symbol = rep(c("Alpha (A)", "Beta (B)"), each = 3),
#'   timestamp = rep(as.Date("2024-01-01") + 0:2, 2),
#'   close = c(10, 11, 12, 200, 180, 220)
#' )
#' normalize_prices(history)
normalize_prices <- function(history) {
  first_date <- as.Date(
    max(tapply(as.numeric(history$timestamp), history$name_symbol, min)),
    origin = "1970-01-01"
  )
  prices <- history[history$timestamp >= first_date, ]
  prices$index <- stats::ave(prices$close, prices$name_symbol, FUN = function(x) x / x[1] * 100)
  prices
}
##########################################################################################
# 
##########################################################################################
#' Monthly returns of a coin
#'
#' Each month's return is its last close relative to the last close of the
#' month before, in percent. The first month, and the months still to come,
#' are `NA`.
#'
#' @param history Price history of one coin from [get_history()], with
#'   `timestamp` and `close` columns, sorted by date.
#'
#' @return A year by month matrix of returns in percent, rounded to 2
#'   decimals, with the years as row names and `month.abb` as column names.
#'
#' @export
#'
#' @examples
#' history <- data.frame(
#'   timestamp = as.Date(c("2024-01-31", "2024-02-29", "2024-03-31")),
#'   close = c(100, 110, 121)
#' )
#' monthly_returns(history)
monthly_returns <- function(history) {
  check_one_coin(history)
  month_close <- tapply(history$close, format(history$timestamp, "%Y-%m"), function(x) x[length(x)])
  month_return <- round((month_close / c(NA, utils::head(month_close, -1)) - 1) * 100, 2)
  months <- names(month_close)
  calendar <- tapply(
    month_return,
    list(substr(months, 1, 4), factor(as.integer(substr(months, 6, 7)), levels = 1:12)),
    sum
  )
  colnames(calendar) <- month.abb
  calendar
}
##########################################################################################
#
##########################################################################################
#' Moving average of prices
#'
#' The simple moving average: each day's value is the mean of the last
#' `days` closes, that day included, so the first `days - 1` days have none.
#'
#' @param close Numeric vector of close prices, sorted by date.
#' @param days Number of days to average, such as 50 or 200.
#'
#' @return A numeric vector as long as `close`, `NA` for the first
#'   `days - 1` days, and all `NA` when there are fewer than `days` closes,
#'   as for a coin younger than the average.
#'
#' @export
#'
#' @examples
#' moving_average(c(1, 2, 3, 4, 5), days = 3)
moving_average <- function(close, days) {
  if (days > length(close)) {
    return(rep(NA_real_, length(close)))
  }
  as.numeric(stats::filter(close, rep(1 / days, days), sides = 1))
}
##########################################################################################
#
##########################################################################################
#' Drawdown of prices
#'
#' How far each close is below the highest close up to that day, the
#' all-time high so far, in percent: 0 on a new high and -50 at half of it.
#'
#' @param close Numeric vector of close prices, sorted by date.
#'
#' @return A numeric vector as long as `close`, from -100 to 0.
#'
#' @export
#'
#' @examples
#' drawdown(c(100, 120, 90, 130, 65))
drawdown <- function(close) {
  (close / cummax(close) - 1) * 100
}
