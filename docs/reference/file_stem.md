# Turn a label into a file name stem

Lower case, with every run of characters other than letters and digits
replaced by a hyphen, so that "Bitcoin (BTC)" becomes "bitcoin-btc".

## Usage

``` r
file_stem(label)
```

## Arguments

- label:

  Character string, such as a coin label.

## Value

A character string that can be used in a file name.

## Examples

``` r
golemcrypto:::file_stem("Bitcoin (BTC)")
#> [1] "bitcoin-btc"
```
