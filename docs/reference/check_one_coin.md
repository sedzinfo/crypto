# Stop when a price history holds more than one coin

For the functions that work on one coin at a time, so that a history of
several coins gives a clear error instead of a wrong result.

## Usage

``` r
check_one_coin(history)
```

## Arguments

- history:

  Price history from
  [`get_history()`](https://sedzinfo.github.io/golemcrypto/reference/get_history.md).

## Value

`history`, invisibly, when it holds at most one coin.

## Examples

``` r
history <- data.frame(name_symbol = c("Alpha (A)", "Beta (B)"))
try(golemcrypto:::check_one_coin(history))
#> Error : history holds 2 coins, keep one of them first, for example history[history$name_symbol == "Alpha (A)", ]
```
