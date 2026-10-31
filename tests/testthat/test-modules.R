light <- reactive("light")

test_that("the timeseries module reports a coin it cannot download", {
  testServer(mod_timeseries_server, args = list(coins = coins, dark_mode = light), {
    expect_message(session$setInputs(coin = "Unknown (X)"), "No coin is labelled")
    expect_error(output$plot, "Data not available for this coin")
    expect_error(output$csv)
    expect_null(session$getReturned()())
  })
})

test_that("the compare module waits for the coin selection, then reports missing data", {
  testServer(mod_compare_server, args = list(coins = coins, dark_mode = light), {
    expect_message(
      {
        session$setInputs(coins = "Unknown (X)", start = as.Date("2024-01-01"))
        session$elapse(2000)
      },
      "No coin is labelled"
    )
    expect_error(output$plot, "Data not available for the selected coins")
  })
})

test_that("the top 100 and treemap modules show the listings and when they were downloaded", {
  downloaded <- reactive(list(data = listings, time = as.POSIXct("2024-03-31 14:32")))
  testServer(mod_top100_server, args = list(listings = downloaded), {
    expect_equal(output$updated, "Data: CoinMarketCap, updated 14:32")
    expect_no_error(output$table)
  })
  testServer(mod_treemap_server, args = list(listings = downloaded, dark_mode = light), {
    expect_equal(output$updated, "Data: CoinMarketCap, updated 14:32")
    expect_no_error(output$plot)
  })
})

test_that("the top 100 and treemap modules say why the listings are missing", {
  problem <- "The app has reached its monthly limit of CoinMarketCap requests"
  unavailable <- reactive(list(data = NULL, time = NULL, problem = problem))
  testServer(mod_top100_server, args = list(listings = unavailable), {
    expect_equal(output$updated, "")
    expect_error(output$table, problem)
    expect_error(output$csv)
  })
  testServer(mod_treemap_server, args = list(listings = unavailable, dark_mode = light), {
    expect_equal(output$updated, "")
    expect_error(output$plot, problem)
    expect_error(output$csv)
  })
  # listings downloaded before the problem stay, with the problem after their time
  kept <- reactive(list(data = listings, time = as.POSIXct("2024-03-31 14:32"), problem = problem))
  testServer(mod_top100_server, args = list(listings = kept), {
    expect_equal(output$updated, paste0("Data: CoinMarketCap, updated 14:32. ", problem))
    expect_no_error(output$table)
  })
})

test_that("file_stem() turns a coin label into a file name", {
  expect_equal(file_stem("Bitcoin (BTC)"), "bitcoin-btc")
  expect_equal(file_stem("Gram (prev. Toncoin) (GRAM)"), "gram-prev-toncoin-gram")
})

test_that("the timeseries and compare CSV downloads hold the prices", {
  local_mocked_bindings(get_history = function(labels, start_date, coins) {
    history[history$name_symbol %in% labels, ]
  })
  testServer(mod_timeseries_server, args = list(coins = coins, dark_mode = light), {
    session$setInputs(coin = "Alpha (A)")
    csv <- utils::read.csv(output$csv)
    expect_equal(names(csv), c("date", "name_symbol", "open", "high", "low", "close", "volume"))
    expect_equal(csv$date, c("2024-01-31", "2024-02-29", "2024-03-01", "2024-03-31"))
  })
  testServer(mod_compare_server, args = list(coins = coins, dark_mode = light), {
    session$setInputs(coins = c("Alpha (A)", "Beta (B)"), start = as.Date("2024-01-01"))
    session$elapse(2000)
    csv <- utils::read.csv(output$csv)
    expect_equal(names(csv), c("date", "name_symbol", "close", "index"))
    expect_equal(csv$index[csv$date == "2024-02-29"], c(100, 100))
  })
})

test_that("the top 100 and treemap CSV downloads hold the listings", {
  # inside testServer() `listings` is the module's argument, so the fixture gets another name
  rows <- listings
  downloaded <- reactive(list(data = rows, time = as.POSIXct("2024-03-31 14:32")))
  testServer(mod_top100_server, args = list(listings = downloaded), {
    csv <- utils::read.csv(output$csv)
    expect_equal(names(csv), gsub("USD_|percent_", "", names(rows)))
    expect_equal(csv$name_symbol, rows$name_symbol)
    expect_equal(csv$price, rows$USD_price)
  })
  testServer(mod_treemap_server, args = list(listings = downloaded, dark_mode = light), {
    csv <- utils::read.csv(output$csv)
    expect_equal(names(csv), c("name_symbol", "market_cap", "change_24h"))
    expect_equal(csv$change_24h, rows$USD_percent_change_24h)
  })
})

test_that("the returns calendar CSV has a year column and one column per month", {
  alpha <- history[history$name_symbol == "Alpha (A)", ]
  testServer(mod_returns_calendar_server, args = list(history = reactive(alpha), dark_mode = light), {
    csv <- utils::read.csv(output$csv)
    expect_equal(names(csv), c("year", month.abb))
    expect_equal(csv$year, 2024)
    expect_equal(c(csv$Jan, csv$Feb, csv$Mar), c(NA, 10, 10))
  })
})

test_that("the returns calendar needs at least two months of history", {
  alpha <- history[history$name_symbol == "Alpha (A)", ]
  testServer(mod_returns_calendar_server, args = list(history = reactive(alpha), dark_mode = light), {
    expect_no_error(output$plot)
  })
  testServer(mod_returns_calendar_server, args = list(history = reactive(alpha[1, ]), dark_mode = light), {
    expect_error(output$plot, "Not enough history")
  })
})

test_that("the about tab explains every tab and every column of the top 100 table", {
  page <- htmltools::renderTags(mod_about_ui("about"))$html
  for (tab in c("Timeseries", "Compare", "Top 100", "Treemap", "Returns calendar")) {
    expect_match(page, paste0(">", tab, "</h3>"), fixed = TRUE, label = tab)
  }
  for (column in setdiff(names(listings_table(listings)$x$data), " ")) {
    expect_match(page, paste0("<code>", column, "</code>"), fixed = TRUE, label = column)
  }
  # the refresh intervals are filled in, and the example chart sits in the text
  expect_no_match(page, "{{", fixed = TRUE)
  expect_match(page, "id=\"about-example\"", fixed = TRUE)
  ids <- regmatches(page, gregexpr("id=\"[^\"]+\"", page))[[1]]
  expect_false(anyDuplicated(ids) > 0)
  testServer(mod_about_server, args = list(dark_mode = light), {
    expect_no_error(output$example)
  })
})
