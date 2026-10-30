# The downloads kept between R sessions

The price histories and the list of coins are kept on disk, so they
survive R restarts: by default in the cache folder of the package from
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html), or in the
folder set with `options(crypto.cache_dir = )`. Delete the folder to
start over.

## Usage

``` r
disk_cache()
```

## Value

A
[`cachem::cache_disk()`](https://cachem.r-lib.org/reference/cache_disk.html)
object.

## Examples

``` r
if (FALSE) { # \dontrun{
golemcrypto:::disk_cache()$keys()
} # }
```
