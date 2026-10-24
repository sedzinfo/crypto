test_that("normalize_prices() starts every coin at 100 on the first common date", {
  prices <- normalize_prices(history)
  expect_equal(min(prices$timestamp), as.Date("2024-02-29"))
  expect_equal(prices$index[prices$name_symbol == "Alpha (A)"], c(100, 90, 110))
  expect_equal(prices$index[prices$name_symbol == "Beta (B)"], c(100, 110, 50))
})

test_that("monthly_returns() compares the last close of each month", {
  calendar <- monthly_returns(history[history$name_symbol == "Alpha (A)", ])
  expect_equal(dimnames(calendar), list("2024", month.abb))
  expect_equal(unname(calendar["2024", c("Jan", "Feb", "Mar")]), c(NA, 10, 10))
  expect_true(all(is.na(calendar["2024", 4:12])))
})

test_that("moving_average() averages the last days once there are enough of them", {
  expect_equal(moving_average(c(1, 2, 3, 4, 5), days = 3), c(NA, NA, 2, 3, 4))
  expect_equal(moving_average(c(1, 2, 3), days = 1), c(1, 2, 3))
  expect_true(all(is.na(moving_average(1:5, days = 10))))
})

test_that("drawdown() measures how far each close is below the high so far", {
  expect_equal(drawdown(c(100, 120, 90, 130, 65)), c(0, 0, -25, 0, -50))
})

test_that("monthly_returns() refuses a history of several coins", {
  expect_error(monthly_returns(history), "holds 2 coins")
})
