##########################################################################################
#
##########################################################################################
#' Minutes between downloads of the market listings
#'
#' @noRd
refresh_minutes <- 10

#' Minutes before a kept price history is brought up to date
#'
#' @noRd
history_minutes <- 60

#' Minutes the list of coins is reused
#'
#' @noRd
coins_minutes <- 24 * 60

#' Cache of the market listings
#'
#' Shared by all sessions and app runs in the same R process, so the API is
#' called at most once per refresh interval.
#'
#' @noRd
listings_cache <- new.env(parent = emptyenv())

#' Folder and object of the disk cache, see `disk_cache()`
#'
#' @noRd
disk <- new.env(parent = emptyenv())
##########################################################################################
#
##########################################################################################
#' The downloads kept between R sessions
#'
#' The price histories and the list of coins are kept on disk, so they
#' survive R restarts: by default in the cache folder of the package from
#' [tools::R_user_dir()], or in the folder set with
#' `options(crypto.cache_dir = )`. Delete the folder to start over.
#'
#' @return A [cachem::cache_disk()] object.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' crypto:::disk_cache()$keys()
#' }
disk_cache <- function() {
  dir <- getOption("crypto.cache_dir", tools::R_user_dir("crypto", which = "cache"))
  if (!identical(disk$dir, dir)) {
    disk$cache <- cachem::cache_disk(dir, max_size = 200 * 1024^2)
    disk$dir <- dir
  }
  disk$cache
}
##########################################################################################
#
##########################################################################################
#' Set up the 'CoinMarketCap' API key
#'
#' Checks that there is a key and passes it to 'coinmarketcapr', which also
#' sets the address of the API.
#'
#' @param api_key A 'CoinMarketCap' API key, by default the
#'   `COINMARKETCAP_APIKEY` environment variable.
#'
#' @return The key, invisibly.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' crypto:::use_api_key()
#' }
use_api_key <- function(api_key = Sys.getenv("COINMARKETCAP_APIKEY")) {
  if (!nzchar(api_key)) {
    stop(
      "A CoinMarketCap API key is needed. Set COINMARKETCAP_APIKEY in your ",
      ".Renviron or pass it as api_key, see the README.",
      call. = FALSE
    )
  }
  coinmarketcapr::setup(api_key = api_key)
  invisible(api_key)
}
##########################################################################################
#
##########################################################################################
#' Label coins as "Name (SYMBOL)"
#'
#' The label identifies coins throughout the package: in the arguments of
#' [get_history()], and in the pickers, tables and plots of the app.
#'
#' @param coins Data frame with `name` and `symbol` columns.
#'
#' @return A character vector with one label per row of `coins`.
#'
#' @keywords internal
#'
#' @examples
#' crypto:::coin_label(data.frame(name = c("Bitcoin", "Ethereum"), symbol = c("BTC", "ETH")))
coin_label <- function(coins) {
  paste0(coins$name, " (", coins$symbol, ")")
}
##########################################################################################
#
##########################################################################################
#' List the active coins
#'
#' Downloads the list of the active coins tracked by 'CoinMarketCap' and
#' keeps it on disk for a day (see [disk_cache()]). When the download fails,
#' the kept list is used. Its `name_symbol` column holds the labels, such as
#' "Bitcoin (BTC)", that pick coins in [get_history()].
#'
#' @return The [crypto2::crypto_list()] data frame, with a `name_symbol`
#'   column.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' head(coins$name_symbol)
#' }
get_coins <- function() {
  kept <- disk_cache()$get("coins")
  if (!cachem::is.key_missing(kept) && minutes_since(kept$time) < coins_minutes) {
    return(kept$coins)
  }
  coins <- tryCatch(crypto2::crypto_list(only_active = TRUE, add_untracked = FALSE), error = function(e) {
    if (cachem::is.key_missing(kept)) {
      stop(e)
    }
    message("crypto_list failed, using the list of coins kept on ", format(kept$time, "%Y-%m-%d"), ": ", conditionMessage(e))
    NULL
  })
  if (is.null(coins)) {
    return(kept$coins)
  }
  coins$name_symbol <- coin_label(coins)
  disk_cache()$set("coins", list(coins = coins, time = Sys.time()))
  coins
}
##########################################################################################
#
##########################################################################################
#' Download the latest market listings
#'
#' Calls the 'CoinMarketCap' API with the key from [use_api_key()].
#'
#' @return A data frame with one row per coin, sorted by market cap, with a
#'   `name_symbol` label and the USD price, volume, price change, market cap
#'   and dominance columns shown in the app.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' listings <- crypto:::fetch_listings()
#' head(listings)
#' }
fetch_listings <- function() {
  use_api_key()
  listings <- coinmarketcapr::get_crypto_listings(currency = "USD", latest = TRUE)
  listings <- listings[order(-listings$USD_market_cap), ]
  listings$name_symbol <- coin_label(listings)
  listings[, c(
    "name_symbol", "USD_price", "USD_volume_24h", "USD_volume_change_24h",
    "USD_percent_change_24h", "USD_percent_change_7d", "USD_percent_change_30d",
    "USD_percent_change_60d", "USD_percent_change_90d", "USD_market_cap",
    "USD_market_cap_dominance", "USD_fully_diluted_market_cap", "USD_last_updated"
  )]
}
##########################################################################################
#
##########################################################################################
#' Get the latest market listings
#'
#' Returns the listings of the coins by market cap, with their price, volume,
#' price changes from 24 hours to 90 days, market cap and dominance in USD.
#' The listings are downloaded again when they are older than 10 minutes,
#' and the download is shared by all sessions and app runs in the same R
#' process, so the API is called at most once in that time.
#'
#' The listings need a 'CoinMarketCap' API key in the `COINMARKETCAP_APIKEY`
#' environment variable, see the README. The first download lets errors
#' through. When a later download fails, the previous listings and their
#' time are kept, so the time stays truthful.
#'
#' @return A list with `data`, a data frame with one row per coin sorted by
#'   market cap, and `time`, when it was downloaded.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' listings <- get_listings()
#' listings$time
#' head(listings$data)
#' }
get_listings <- function() {
  if (is.null(listings_cache$data)) {
    listings_cache$data <- fetch_listings()
    listings_cache$time <- Sys.time()
  } else if (minutes_since(listings_cache$time) >= refresh_minutes) {
    listings <- tryCatch(fetch_listings(), error = function(e) {
      message("get_crypto_listings failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(listings)) {
      listings_cache$data <- listings
      listings_cache$time <- Sys.time()
    }
  }
  list(data = listings_cache$data, time = listings_cache$time)
}
##########################################################################################
#
##########################################################################################
#' Download the daily price history of a coin
#'
#' Downloads the daily USD prices with [crypto2::crypto_history()], in
#' requests of 400 days. A failed download is reported as a message instead
#' of an error.
#'
#' @param coin One row of [get_coins()].
#' @param start_date Date. The first day to download.
#'
#' @return The price history of the coin, as described in [get_history()],
#'   or `NULL` when nothing could be downloaded.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' crypto:::download_history(coins[coins$name_symbol == "Bitcoin (BTC)", ], Sys.Date() - 30)
#' }
download_history <- function(coin, start_date) {
  tryCatch(
    {
      history <- crypto2::crypto_history(
        coin_list = coin,
        convert = "USD",
        limit = NULL,
        start_date = format(start_date, "%Y%m%d")
      )
      if (is.null(history) || nrow(history) == 0) {
        return(NULL)
      }
      history <- as.data.frame(history)
      history$timestamp <- as.Date(as.character(history$timestamp))
      history$name_symbol <- coin_label(history)
      # 'CoinMarketCap' sends up to 400 days ending at the end of each request,
      # so the last request of crypto2 repeats days of the one before
      history <- history[!duplicated(history$timestamp), ]
      history[order(history$timestamp), ]
    },
    error = function(e) {
      message("crypto_history failed: ", conditionMessage(e))
      NULL
    }
  )
}
##########################################################################################
#
##########################################################################################
#' Check a price history for missing days
#'
#' When 'CoinMarketCap' blocks the same request twice, crypto2 leaves its 400
#' days out of the download, which shows as a gap in the dates of a coin.
#'
#' @param history Price history from [download_history()], sorted by coin
#'   and date.
#'
#' @return `TRUE` when a day is missing between the first and the last day
#'   of any coin.
#'
#' @keywords internal
#'
#' @examples
#' history <- data.frame(
#'   name_symbol = "Alpha (A)",
#'   timestamp = as.Date(c("2024-01-01", "2024-01-02", "2024-01-05"))
#' )
#' crypto:::has_gaps(history)
has_gaps <- function(history) {
  any(tapply(history$timestamp, history$name_symbol, function(dates) any(diff(dates) > 1)))
}
##########################################################################################
#
##########################################################################################
#' Keep a downloaded price history on disk
#'
#' A history with missing days (see [has_gaps()]) is returned but not kept,
#' so the next call downloads it again.
#'
#' @param key Key of the coin in [disk_cache()].
#' @param history Price history of the coin, or `NULL` when the download
#'   failed.
#' @param from Date. The first day asked for, which can be earlier than the
#'   first day of the coin.
#'
#' @return A list with the `history`, `from` and the `time` of the download,
#'   or `NULL` when `history` is `NULL`.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' btc <- coins[coins$name_symbol == "Bitcoin (BTC)", ]
#' crypto:::keep_history("history_1", crypto:::download_history(btc, Sys.Date() - 30), Sys.Date() - 30)
#' }
keep_history <- function(key, history, from) {
  if (is.null(history)) {
    return(NULL)
  }
  kept <- list(history = history, from = from, time = Sys.time())
  if (has_gaps(history)) {
    message("crypto_history left days out, the download is not kept")
  } else {
    disk_cache()$set(key, kept)
  }
  kept
}
##########################################################################################
#
##########################################################################################
#' Get the price history of one coin, kept on disk and brought up to date
#'
#' The first call downloads the history from `start_date` and keeps it on
#' disk (see [disk_cache()]). Later calls use the kept history, and once it
#' is more than an hour old download only the days since its last day,
#' because past days do not change: one request instead of one per 400 days.
#' A history kept from a later day than `start_date` is downloaded again in
#' full, and when bringing it up to date fails, the kept history is used.
#'
#' @param coin One row of [get_coins()].
#' @param start_date Date. The first day needed.
#'
#' @return The price history of the coin from `start_date`, or `NULL` when it
#'   could not be downloaded.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' crypto:::coin_history(coins[coins$name_symbol == "Bitcoin (BTC)", ], Sys.Date() - 30)
#' }
coin_history <- function(coin, start_date) {
  key <- paste0("history_", coin$id)
  kept <- disk_cache()$get(key)
  if (cachem::is.key_missing(kept) || kept$from > start_date) {
    kept <- keep_history(key, download_history(coin, start_date), start_date)
  } else if (minutes_since(kept$time) >= history_minutes) {
    # the last kept day is downloaded again, as it may have been incomplete
    recent <- download_history(coin, max(kept$history$timestamp) - 1)
    if (!is.null(recent)) {
      older <- kept$history[kept$history$timestamp < min(recent$timestamp), ]
      kept <- keep_history(key, rbind(older, recent), kept$from)
    }
  }
  if (is.null(kept)) {
    return(NULL)
  }
  history <- kept$history[kept$history$timestamp >= start_date, ]
  # histories kept by earlier versions can still have repeated days
  history[!duplicated(history$timestamp), ]
}
##########################################################################################
#
##########################################################################################
#' Get the daily price history of coins
#'
#' Returns the daily USD prices of one or more coins from 'CoinMarketCap'.
#' Each coin's history is kept on disk between R sessions, and once it is
#' more than an hour old only the days since its last day are downloaded, so
#' calls are fast and stay below the request limit of 'CoinMarketCap'. When
#' that limit is hit, crypto2 waits a minute and tries again.
#'
#' @param labels Character vector of coin labels, such as "Bitcoin (BTC)",
#'   from the `name_symbol` column of [get_coins()].
#' @param start_date Date. The first day to get, a year ago by default.
#' @param coins Data frame of coins from [get_coins()], to look the labels up.
#'
#' @return A data frame with one row per coin and day, sorted by coin and
#'   date, with the `open`, `high`, `low`, `close` and `volume` columns,
#'   `timestamp` as a Date and the `name_symbol` label. Labels that match no
#'   coin are left out with a message, and `NULL` is returned when no label
#'   matches or nothing could be downloaded.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' btc <- get_history("Bitcoin (BTC)", as.Date("2020-01-01"))
#' plot_timeseries(btc)
#'
#' history <- get_history(c("Bitcoin (BTC)", "Ethereum (ETH)"))
#' plot_compare(normalize_prices(history))
#' }
get_history <- function(labels, start_date = Sys.Date() - 365, coins = get_coins()) {
  unknown <- setdiff(labels, coins$name_symbol)
  if (length(unknown) > 0) {
    message("No coin is labelled ", paste0("\"", unknown, "\"", collapse = ", "), ", see get_coins()$name_symbol")
  }
  matched <- coins[match(unique(labels), coins$name_symbol, nomatch = 0), ]
  history <- do.call(rbind, lapply(seq_len(nrow(matched)), function(i) coin_history(matched[i, ], start_date)))
  if (is.null(history) || nrow(history) == 0) {
    return(NULL)
  }
  history <- history[order(history$name_symbol, history$timestamp), ]
  rownames(history) <- NULL
  history
}
