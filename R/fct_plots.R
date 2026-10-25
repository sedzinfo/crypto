##########################################################################################
# 
##########################################################################################
#' Date range buttons of the timeseries chart
#'
#' @noRd
range_buttons <- list(
  list(count = 1, label = "1 year", step = "year", stepmode = "backward"),
  list(count = 1, label = "year to date", step = "year", stepmode = "todate"),
  list(count = 6, label = "6 months", step = "month", stepmode = "backward"),
  list(count = 3, label = "3 months", step = "month", stepmode = "backward"),
  list(count = 1, label = "1 month", step = "month", stepmode = "backward"),
  list(count = 7, label = "7 days", step = "day", stepmode = "backward"),
  list(step = "all")
)

#' Colors of the moving average lines, in the order of the averages
#'
#' Blue and purple first, which stand out from the orange close line, the
#' green and red candles and the green volume, in light and dark mode.
#'
#' @noRd
average_colors <- c("#3b82f6", "#a855f7", "#14b8a6", "#eab308")
##########################################################################################
#
##########################################################################################
#' Candlestick chart of a coin's prices
#'
#' Plots the daily open, high, low and close prices, with a line through the
#' close, its moving averages, date range buttons and a range slider.
#' Underneath, smaller charts show the trading volume and the drawdown: how
#' far the price is below its all-time high so far. A click on a legend entry
#' hides or shows that line.
#'
#' @param history Price history of one coin from [get_history()]. The
#'   averages and the drawdown use the whole history, so pass all of it and
#'   zoom with the range buttons.
#' @param mode `"light"` (the default) or `"dark"`, to suit a light or dark
#'   page.
#' @param averages Days of the moving averages to draw (see
#'   [moving_average()]), or `NULL` for none.
#' @param show_drawdown `TRUE` to add the drawdown chart (see [drawdown()]).
#'
#' @return A plotly object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' btc <- get_history("Bitcoin (BTC)", as.Date("2013-04-29"))
#' plot_timeseries(btc)
#'
#' # shorter averages, and no drawdown chart
#' plot_timeseries(btc, averages = c(20, 100), show_drawdown = FALSE)
#' }
plot_timeseries <- function(history, mode = "light", averages = c(50, 200), show_drawdown = TRUE) {
  check_one_coin(history)
  hover_text <- paste0(
    "\nDate=", history$timestamp,
    "\nClose=", history$close,
    "\nOpen=", history$open,
    "\nHigh=", history$high,
    "\nLow=", history$low
  )
  price <- plotly::plot_ly(history,
    x = history$timestamp,
    open = history$open,
    high = history$high,
    low = history$low,
    close = history$close,
    yaxis = "y",
    text = hover_text,
    name = "Range",
    type = "candlestick"
  ) %>%
    plotly::add_lines(
      x = history$timestamp,
      y = history$close,
      line = list(width = 1, color = "#ff7f0e"),
      inherit = FALSE,
      text = hover_text,
      name = "close"
    )
  for (i in seq_along(averages)) {
    price <- plotly::add_lines(price,
      x = history$timestamp,
      y = moving_average(history$close, averages[i]),
      line = list(width = 1.5, color = average_colors[(i - 1) %% length(average_colors) + 1]),
      inherit = FALSE,
      name = paste0(averages[i], "-day average"),
      hovertemplate = paste0("%{x}<br>", averages[i], "-day average: %{y:,.2f}<extra></extra>")
    )
  }
  price <- price %>%
    plotly::layout(
      # on the left, as the legend is centred above the chart
      title = list(text = unique(history$name), x = 0.01, xanchor = "left"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center"),
      yaxis = list(title = "USD"),
      xaxis = list(
        title = "Date",
        rangeslider = list(type = "date", visible = TRUE),
        rangeselector = c(button_colors(mode), list(buttons = range_buttons))
      )
    )
  volume <- plotly::plot_ly(
    x = history$timestamp,
    y = history$volume,
    type = "scatter",
    mode = "lines",
    line = list(color = "#2ca02c"),
    name = "Volume"
  ) %>%
    # no axis title: the legend names the panel, and on a short panel the titles run into each other
    plotly::layout(yaxis = list(title = ""))
  panels <- list(price, volume)
  heights <- c(0.8, 0.2)
  if (show_drawdown) {
    from_high <- plotly::plot_ly(
      x = history$timestamp,
      y = drawdown(history$close),
      type = "scatter",
      mode = "lines",
      fill = "tozeroy",
      line = list(width = 1, color = change_color(1, gain = FALSE)),
      fillcolor = change_color(0.2, gain = FALSE),
      name = "From high",
      hovertemplate = "%{x}<br>From high: %{y:.1f}%<extra></extra>"
    ) %>%
      plotly::layout(yaxis = list(title = "", ticksuffix = "%"))
    panels <- c(panels, list(from_high))
    heights <- c(0.65, 0.15, 0.2)
  }
  plotly::subplot(panels, heights = heights, nrows = length(panels), shareX = TRUE, titleY = TRUE) %>%
    plot_theme(mode)
}
##########################################################################################
# 
##########################################################################################
#' Line chart of normalized close prices
#'
#' One line per coin, with buttons that switch the y axis between a linear
#' and a log scale.
#'
#' @param prices Prices from [normalize_prices()].
#' @inheritParams plot_timeseries
#'
#' @return A plotly object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' history <- get_history(c("Bitcoin (BTC)", "Ethereum (ETH)", "Solana (SOL)"))
#' plot_compare(normalize_prices(history))
#' }
plot_compare <- function(prices, mode = "light") {
  plotly::plot_ly(prices,
    x = ~timestamp,
    y = ~index,
    color = ~name_symbol,
    type = "scatter",
    mode = "lines",
    hovertemplate = "%{x}<br>%{y:.2f}"
  ) %>%
    plotly::layout(
      title = paste0("Normalized close (", min(prices$timestamp), " = 100)"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center"),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Index"),
      updatemenus = list(list(
        type = "buttons",
        direction = "right",
        x = 0,
        y = 1.08,
        # plotly draws the active button light in both modes, so the other buttons get a
        # light background too, instead of the page's, and all of them dark text
        bgcolor = "#eeeeee",
        font = list(color = "#212529"),
        buttons = list(
          list(label = "Linear", method = "relayout", args = list("yaxis.type", "linear")),
          list(label = "Log", method = "relayout", args = list("yaxis.type", "log"))
        )
      ))
    ) %>%
    plot_theme(mode)
}
##########################################################################################
# 
##########################################################################################
#' Treemap of the top 100 coins
#'
#' Each box is sized by market cap and colored from red to green by the 24h
#' price change. The color limits are the 5th and 95th percentiles of the
#' changes, so a few outliers do not wash out the rest. Coins without a 24h
#' change are labelled n/a and colored white.
#'
#' @param listings The `data` of [get_listings()].
#' @inheritParams plot_timeseries
#'
#' @return A plotly object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_treemap(get_listings()$data)
#' }
plot_treemap <- function(listings, mode = "light") {
  top <- utils::head(listings, 100)
  market_cap <- top$USD_market_cap
  market_cap[is.na(market_cap)] <- 0
  change <- top$USD_percent_change_24h
  limit <- max(abs(stats::quantile(change, c(.05, .95), na.rm = TRUE)))
  # %{color} in a plotly template is the color string, so the label text is built here
  change_text <- ifelse(is.na(change), "n/a", sprintf("%.2f%%", change))
  change[is.na(change)] <- 0
  n <- nrow(top)
  plotly::plot_ly(
    type = "treemap",
    ids = c("root", as.character(seq_len(n))),
    labels = c("Top 100", top$name_symbol),
    parents = c("", rep("root", n)),
    values = c(sum(market_cap), market_cap),
    branchvalues = "total",
    marker = list(
      colors = c(0, change),
      colorscale = colorscale_change,
      cmin = -limit,
      cmax = limit,
      showscale = TRUE
    ),
    text = c("", change_text),
    texttemplate = "%{label}<br>%{text}",
    # plotly picks label colors as if the page were white, which hides them in dark mode
    textfont = list(color = text_color(mode)),
    hovertemplate = "%{label}<br>Market cap: $%{value:,.0f}<br>24h: %{text}<extra></extra>"
  ) %>%
    plotly::layout(title = "Top 100 by market cap, colored by 24h change") %>%
    plot_theme(mode)
}
##########################################################################################
# 
##########################################################################################
#' Heatmap of monthly returns by year
#'
#' Months are colored from red to green by their return. The color limit is
#' the 95th percentile of the absolute returns, so a few extreme months do
#' not wash out the rest.
#'
#' @param calendar Matrix from [monthly_returns()].
#' @param label Label of the coin for the title, or `NULL` for none.
#' @inheritParams plot_timeseries
#'
#' @return A plotly object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' btc <- get_history("Bitcoin (BTC)", as.Date("2013-04-29"))
#' plot_returns_calendar(monthly_returns(btc), "Bitcoin (BTC)")
#' }
plot_returns_calendar <- function(calendar, label = NULL, mode = "light") {
  limit <- unname(stats::quantile(abs(calendar), .95, na.rm = TRUE))
  plotly::plot_ly(
    x = colnames(calendar),
    y = rownames(calendar),
    z = calendar,
    type = "heatmap",
    zmin = -limit,
    zmax = limit,
    colorscale = colorscale_change,
    hovertemplate = "%{x} %{y}<br>%{z:.2f}%<extra></extra>"
  ) %>%
    plotly::layout(
      title = paste(c("Monthly returns", label), collapse = ": "),
      # the grid would show through the faint and the empty months
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "", type = "category", showgrid = FALSE)
    ) %>%
    plot_theme(mode)
}
##########################################################################################
# 
##########################################################################################
#' Table of the market listings
#'
#' A sortable table with 100 rows per page and numbers rounded to 2
#' decimals, showing the price in dollars and the price changes in percent.
#' Losses are shaded red and gains green, more strongly the larger the
#' change, in steps of 0.1 percentile from the 5th to the 95th. Changes near
#' zero keep the table's own background, so the table suits the light and the
#' dark mode.
#'
#' @param listings The `data` of [get_listings()].
#'
#' @return A DT datatable.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' listings_table(get_listings()$data)
#' }
listings_table <- function(listings) {
  names(listings) <- gsub("USD_|percent_", "", names(listings))
  listings <- data.frame(listings, check.names = FALSE)
  numeric_columns <- vapply(listings, is.numeric, logical(1))
  listings[numeric_columns] <- round(listings[numeric_columns], 2)

  change_columns <- c("change_24h", "change_7d", "change_30d", "change_60d", "change_90d")
  breaks <- stats::quantile(unlist(listings[change_columns]), probs = seq(.05, .95, .001), na.rm = TRUE)
  colors <- c(
    change_color(seq(1, 0, length.out = sum(breaks < 0)), gain = FALSE),
    change_color(seq(0, 1, length.out = sum(breaks >= 0) + 1), gain = TRUE)
  )

  DT::datatable(listings, options = list(
    stateSave = TRUE,
    pageLength = 100,
    lengthMenu = list(c(20, 50, 100, -1), c("20", "50", "100", "All"))
  )) %>%
    DT::formatStyle(change_columns, backgroundColor = DT::styleInterval(breaks, colors)) %>%
    DT::formatCurrency("price", currency = "$", digits = 4) %>%
    DT::formatCurrency(change_columns, currency = "%", before = FALSE, digits = 2)
}
