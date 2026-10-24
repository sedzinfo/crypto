daily <- data.frame(
  name_symbol = "Alpha (A)",
  timestamp = as.Date("2024-01-01") + 0:9,
  close = 1:10
)

make_stale <- function(key, minutes) {
  kept <- disk_cache()$get(key)
  kept$time <- kept$time - minutes * 60
  disk_cache()$set(key, kept)
}

test_that("use_api_key() asks for a key when there is none", {
  expect_error(use_api_key(""), "API key is needed")
})

test_that("get_coins() keeps the list of coins on disk for a day", {
  disk_cache()$reset()
  downloads <- 0
  local_mocked_bindings(
    crypto_list = function(...) {
      downloads <<- downloads + 1
      coins[c("id", "name", "symbol")]
    },
    .package = "crypto2"
  )
  expect_equal(get_coins()$name_symbol, coins$name_symbol)
  get_coins()
  expect_equal(downloads, 1)

  make_stale("coins", coins_minutes)
  get_coins()
  expect_equal(downloads, 2)
})

test_that("get_coins() uses the kept list when the download fails", {
  disk_cache()$reset()
  local_mocked_bindings(crypto_list = function(...) stop("offline"), .package = "crypto2")
  expect_error(get_coins(), "offline")

  disk_cache()$set("coins", list(coins = coins, time = Sys.time() - 2 * coins_minutes * 60))
  expect_message(kept <- get_coins(), "using the list of coins kept")
  expect_equal(kept, coins)
})

test_that("get_history() names the labels that match no coin", {
  expect_message(result <- get_history("Unknown (X)", coins = coins), "No coin is labelled \"Unknown \\(X\\)\"")
  expect_null(result)
})

test_that("has_gaps() finds a missing day in any coin", {
  expect_false(has_gaps(daily))
  expect_true(has_gaps(daily[-5, ]))
  expect_true(has_gaps(history))
})

test_that("get_history() keeps a history on disk and later downloads only the new days", {
  disk_cache()$reset()
  available <- daily
  starts <- character()
  local_mocked_bindings(download_history = function(coin, start_date) {
    starts <<- c(starts, format(start_date))
    available[available$timestamp >= start_date, ]
  })

  expect_equal(nrow(get_history("Alpha (A)", as.Date("2024-01-01"), coins)), 10)
  later <- get_history("Alpha (A)", as.Date("2024-01-05"), coins)
  expect_equal(starts, "2024-01-01")
  expect_equal(later$timestamp, as.Date("2024-01-05") + 0:5)

  # an hour later, with two new days and a corrected last day
  available <- rbind(daily, data.frame(name_symbol = "Alpha (A)", timestamp = as.Date("2024-01-11") + 0:1, close = 11:12))
  available$close[available$timestamp == as.Date("2024-01-10")] <- 99
  make_stale("history_1", history_minutes)

  updated <- get_history("Alpha (A)", as.Date("2024-01-01"), coins)
  expect_equal(starts, c("2024-01-01", "2024-01-09"))
  expect_equal(updated$timestamp, as.Date("2024-01-01") + 0:11)
  expect_equal(updated$close[updated$timestamp == as.Date("2024-01-10")], 99)
})

test_that("get_history() downloads the whole history again for an earlier start date", {
  disk_cache()$reset()
  starts <- character()
  local_mocked_bindings(download_history = function(coin, start_date) {
    starts <<- c(starts, format(start_date))
    daily[daily$timestamp >= start_date, ]
  })
  get_history("Alpha (A)", as.Date("2024-01-05"), coins)
  get_history("Alpha (A)", as.Date("2024-01-01"), coins)
  expect_equal(starts, c("2024-01-05", "2024-01-01"))
})

test_that("get_history() uses the kept history when bringing it up to date fails", {
  disk_cache()$reset()
  offline <- FALSE
  local_mocked_bindings(download_history = function(coin, start_date) {
    if (offline) NULL else daily[daily$timestamp >= start_date, ]
  })
  get_history("Alpha (A)", as.Date("2024-01-01"), coins)
  make_stale("history_1", history_minutes)
  offline <- TRUE
  expect_equal(nrow(get_history("Alpha (A)", as.Date("2024-01-01"), coins)), 10)
})

test_that("get_history() does not keep a download with missing days", {
  disk_cache()$reset()
  downloads <- 0
  local_mocked_bindings(download_history = function(coin, start_date) {
    downloads <<- downloads + 1
    daily[-5, ]
  })
  expect_message(get_history("Alpha (A)", as.Date("2024-01-01"), coins), "not kept")
  expect_message(get_history("Alpha (A)", as.Date("2024-01-01"), coins), "not kept")
  expect_equal(downloads, 2)
  expect_false(disk_cache()$exists("history_1"))
})

test_that("download_history() drops the days that crypto2 sends twice", {
  sent <- data.frame(name = "Alpha", symbol = "A", timestamp = as.character(daily$timestamp), close = daily$close)
  local_mocked_bindings(crypto_history = function(...) rbind(sent[8:10, ], sent), .package = "crypto2")
  downloaded <- download_history(coins[1, ], as.Date("2024-01-01"))
  expect_equal(downloaded$timestamp, daily$timestamp)
  expect_equal(downloaded$close, daily$close)
})

test_that("get_history() drops repeated days from a history kept by an earlier version", {
  disk_cache()$reset()
  disk_cache()$set("history_1", list(history = rbind(daily, daily[9:10, ]), from = as.Date("2024-01-01"), time = Sys.time()))
  expect_equal(get_history("Alpha (A)", as.Date("2024-01-01"), coins)$timestamp, daily$timestamp)
})

test_that("get_history() combines several coins and leaves out the ones that fail", {
  disk_cache()$reset()
  local_mocked_bindings(download_history = function(coin, start_date) {
    if (coin$name_symbol == "Beta (B)") NULL else daily
  })
  result <- get_history(c("Beta (B)", "Alpha (A)"), as.Date("2024-01-01"), coins)
  expect_equal(unique(result$name_symbol), "Alpha (A)")
  expect_equal(rownames(result), as.character(1:10))
})
