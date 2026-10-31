##########################################################################################
#
##########################################################################################
#' @title Minutes between downloads of the market listings
#' @noRd
refresh_minutes <- 10
##########################################################################################
#
##########################################################################################
#' @title Minutes before a kept price history is brought up to date
#' @noRd
history_minutes <- 60
##########################################################################################
#
##########################################################################################
#' @title Minutes the list of coins is reused
#' @noRd
coins_minutes <- 24 * 60
##########################################################################################
#
##########################################################################################
#' @title Cache of the market listings
#' @description Shared by all sessions and app runs in the same R process, so
#' the API is called at most once per refresh interval.
#' @noRd
listings_cache <- new.env(parent = emptyenv())
##########################################################################################
#
##########################################################################################
#' @title Folder and object of the disk cache, see `disk_cache()`
#' @noRd
disk <- new.env(parent = emptyenv())
##########################################################################################
#
##########################################################################################
#' @title The downloads kept between R sessions
#' @description The price histories and the list of coins are kept on disk, so
#' they survive R restarts: by default in the cache folder of the package from
#' [tools::R_user_dir()], or in the folder set with
#' `options(crypto.cache_dir = )`. Delete the folder to start over.
#' @return A [cachem::cache_disk()] object.
#' @keywords internal
#' @examples
#' \dontrun{
#' golemcrypto:::disk_cache()$keys()
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
#' @title Set up the 'CoinMarketCap' API key
#' @description Checks that there is a key and passes it to 'coinmarketcapr',
#' which also sets the address of the API.
#' @param api_key A 'CoinMarketCap' API key, by default the
#'   `COINMARKETCAP_APIKEY` environment variable.
#' @return The key, invisibly.
#' @keywords internal
#' @examples
#' \dontrun{
#' golemcrypto:::use_api_key()
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
#' @title Label coins as "Name (SYMBOL)"
#' @description The label identifies coins throughout the package: in the
#' arguments of [get_history()], and in the pickers, tables and plots of the
#' app.
#' @param coins Data frame with `name` and `symbol` columns.
#' @return A character vector with one label per row of `coins`.
#' @keywords internal
#' @examples
#' golemcrypto:::coin_label(data.frame(name = c("Bitcoin", "Ethereum"), symbol = c("BTC", "ETH")))
coin_label <- function(coins) {
  paste0(coins$name, " (", coins$symbol, ")")
}
##########################################################################################
#
##########################################################################################
#' @title List the active coins
#' @description Downloads the list of the active coins tracked by
#' 'CoinMarketCap' and keeps it on disk for a day (see [disk_cache()]). When the
#' download fails, the kept list is used. Its `name_symbol` column holds the
#' labels, such as "Bitcoin (BTC)", that pick coins in [get_history()].
#' @return The [crypto2::crypto_list()] data frame, with a `name_symbol`
#'   column.
#' @export
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
#' @title Download the latest market listings
#' @description Calls the 'CoinMarketCap' API with the key from [use_api_key()].
#' @return A data frame with one row per coin, sorted by market cap, with a
#'   `name_symbol` label and the USD price, volume, price change, market cap
#'   and dominance columns shown in the app.
#' @keywords internal
#' @examples
#' \dontrun{
#' listings <- golemcrypto:::fetch_listings()
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
#' @title Ask 'CoinMarketCap' about the API key
#' @description Calls the key/info endpoint, which uses no credits, for the plan
#' of the key from [use_api_key()] and the credits it has used and has left.
#' @return The answer as a list with `status` and, for a valid key, `data`,
#'   or `NULL` when 'CoinMarketCap' cannot be reached.
#' @keywords internal
#' @examples
#' \dontrun{
#' golemcrypto:::key_info()$data$usage$current_month
#' }
key_info <- function() {
  handle <- curl::new_handle(timeout = 10)
  curl::handle_setheaders(handle, "X-CMC_PRO_API_KEY" = Sys.getenv("COINMARKETCAP_APIKEY"))
  tryCatch(
    jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory("https://pro-api.coinmarketcap.com/v1/key/info", handle = handle)$content)),
    error = function(e) NULL
  )
}
##########################################################################################
#
##########################################################################################
#' @title Why the market listings cannot be downloaded
#' @description Tells the visitors of the app when the API key has used up its
#' credits for the month or the day, and when the limit resets. For any other
#' failure, it says that the download is tried again in 10 minutes.
#' @param info The answer of [key_info()].
#' @return A character string for the visitors of the app.
#' @keywords internal
#' @examples
#' info <- list(
#'   status = list(error_code = 0),
#'   data = list(
#'     plan = list(credit_limit_monthly_reset_timestamp = "2026-11-01T00:00:00.000Z"),
#'     usage = list(current_month = list(credits_left = 0))
#'   )
#' )
#' golemcrypto:::listings_problem(info)
listings_problem <- function(info = key_info()) {
  usage <- info$data$usage
  # 'CoinMarketCap' answers a key over its monthly limit with 1010 and over its daily limit with 1009
  code <- as.character(info$status$error_code)
  if (isTRUE(usage$current_month$credits_left <= 0) || identical(code, "1010")) {
    stamp <- info$data$plan$credit_limit_monthly_reset_timestamp
    reset <- if (is.character(stamp)) as.POSIXct(stamp, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS") else NA
    when <- if (is.na(reset)) {
      "next month"
    } else {
      paste0("on ", as.integer(format(reset, "%d")), " ", month.name[as.integer(format(reset, "%m"))], format(reset, " %Y at %H:%M UTC"))
    }
    paste0(
      "The app has reached its monthly limit of CoinMarketCap requests, so the market data ",
      "cannot be downloaded until the limit resets ", when, ". Please come back then."
    )
  } else if (isTRUE(usage$current_day$credits_left <= 0) || identical(code, "1009")) {
    paste0(
      "The app has reached its daily limit of CoinMarketCap requests, so the market data ",
      "cannot be downloaded until the limit resets at 00:00 UTC. Please come back then."
    )
  } else {
    paste0(
      "The market data cannot be downloaded from CoinMarketCap right now. ",
      "The app tries again every ", refresh_minutes, " minutes."
    )
  }
}
##########################################################################################
#
##########################################################################################
#' @title Get the latest market listings
#' @description Returns the listings of the coins by market cap, with their
#' price, volume, price changes from 24 hours to 90 days, market cap and
#' dominance in USD. The listings are downloaded again when they are older than
#' 10 minutes, and the download is shared by all sessions and app runs in the
#' same R process, so the API is called at most once in that time.
#' @details The listings need a 'CoinMarketCap' API key in the
#' `COINMARKETCAP_APIKEY` environment variable, see the README. A failed
#' download, for example when the key has used up its credits, is tried again
#' after 10 minutes. Until then the previous listings and their time are kept,
#' so the time stays truthful, and `problem` tells the visitors of the app why,
#' see [listings_problem()].
#' @return A list with `data`, a data frame with one row per coin sorted by
#'   market cap, and `time`, when it was downloaded (both `NULL` until a
#'   download works), and `problem`, `NULL` when the last download worked
#'   and otherwise a sentence on why it failed.
#' @export
#' @examples
#' \dontrun{
#' listings <- get_listings()
#' listings$time
#' head(listings$data)
#' }
get_listings <- function() {
  if (is.null(listings_cache$tried) || minutes_since(listings_cache$tried) >= refresh_minutes) {
    listings_cache$tried <- Sys.time()
    listings <- tryCatch(fetch_listings(), error = function(e) {
      message("get_crypto_listings failed: ", conditionMessage(e))
      NULL
    })
    if (is.null(listings)) {
      listings_cache$problem <- listings_problem()
    } else {
      listings_cache$data <- listings
      listings_cache$time <- Sys.time()
      listings_cache$problem <- NULL
    }
  }
  list(data = listings_cache$data, time = listings_cache$time, problem = listings_cache$problem)
}
##########################################################################################
#
##########################################################################################
#' @title Download the daily price history of a coin
#' @description Downloads the daily USD prices with [crypto2::crypto_history()],
#' in requests of 400 days. A failed download is reported as a message instead
#' of an error.
#' @param coin One row of [get_coins()].
#' @param start_date Date. The first day to download.
#' @return The price history of the coin, as described in [get_history()],
#'   or `NULL` when nothing could be downloaded.
#' @keywords internal
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' golemcrypto:::download_history(coins[coins$name_symbol == "Bitcoin (BTC)", ], Sys.Date() - 30)
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
#' @title Check a price history for missing days
#' @description When 'CoinMarketCap' blocks the same request twice, crypto2
#' leaves its 400 days out of the download, which shows as a gap in the dates of
#' a coin.
#' @param history Price history from [download_history()], sorted by coin
#'   and date.
#' @return `TRUE` when a day is missing between the first and the last day
#'   of any coin.
#' @keywords internal
#' @examples
#' history <- data.frame(
#'   name_symbol = "Alpha (A)",
#'   timestamp = as.Date(c("2024-01-01", "2024-01-02", "2024-01-05"))
#' )
#' golemcrypto:::has_gaps(history)
has_gaps <- function(history) {
  any(tapply(history$timestamp, history$name_symbol, function(dates) any(diff(dates) > 1)))
}
##########################################################################################
#
##########################################################################################
#' @title Keep a downloaded price history on disk
#' @description A history with missing days (see [has_gaps()]) is returned but
#' not kept, so the next call downloads it again.
#' @param key Key of the coin in [disk_cache()].
#' @param history Price history of the coin, or `NULL` when the download
#'   failed.
#' @param from Date. The first day asked for, which can be earlier than the
#'   first day of the coin.
#' @return A list with the `history`, `from` and the `time` of the download,
#'   or `NULL` when `history` is `NULL`.
#' @keywords internal
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' btc <- coins[coins$name_symbol == "Bitcoin (BTC)", ]
#' golemcrypto:::keep_history("history_1", golemcrypto:::download_history(btc, Sys.Date() - 30), Sys.Date() - 30)
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
#' @title Get the price history of one coin, kept on disk and brought up to date
#' @description The first call downloads the history from `start_date` and keeps
#' it on disk (see [disk_cache()]). Later calls use the kept history, and once
#' it is more than an hour old download only the days since its last day,
#' because past days do not change: one request instead of one per 400 days. A
#' history kept from a later day than `start_date` is downloaded again in full,
#' and when bringing it up to date fails, the kept history is used.
#' @param coin One row of [get_coins()].
#' @param start_date Date. The first day needed.
#' @return The price history of the coin from `start_date`, or `NULL` when it
#'   could not be downloaded.
#' @keywords internal
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' golemcrypto:::coin_history(coins[coins$name_symbol == "Bitcoin (BTC)", ], Sys.Date() - 30)
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
#' @title Get the daily price history of coins
#' @description Returns the daily USD prices of one or more coins from
#' 'CoinMarketCap'. Each coin's history is kept on disk between R sessions, and
#' once it is more than an hour old only the days since its last day are
#' downloaded, so calls are fast and stay below the request limit of
#' 'CoinMarketCap'. When that limit is hit, crypto2 waits a minute and tries
#' again.
#' @param labels Character vector of coin labels, such as "Bitcoin (BTC)",
#'   from the `name_symbol` column of [get_coins()].
#' @param start_date Date. The first day to get, a year ago by default.
#' @param coins Data frame of coins from [get_coins()], to look the labels up.
#' @return A data frame with one row per coin and day, sorted by coin and
#'   date, with the `open`, `high`, `low`, `close` and `volume` columns,
#'   `timestamp` as a Date and the `name_symbol` label. Labels that match no
#'   coin are left out with a message, and `NULL` is returned when no label
#'   matches or nothing could be downloaded.
#' @export
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
