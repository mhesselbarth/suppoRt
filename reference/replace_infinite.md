# replace_infinite

Replace infinite

## Usage

``` r
replace_infinite(x, what = c("NaN", "Inf"), value = c(NA, NA), verbose = TRUE)
```

## Arguments

- x:

  vector.

- what:

  What infinite values should be replaced. See details.

- value:

  Value that will be used to replace selected infinite values.

- verbose:

  Print warning messages.

## Value

vector

## Details

The function converts all `NaN` and `Inf` values to `NA`. Input must be
a vector. Can be either `what = c("NaN", "Inf")` to replace both `NaN`
and `Inf` or just one of the two, e.g. `what = "NaN"`.

## Examples

``` r
vec <- c(1, 2, 3, NA, 5, NaN, Inf)
replace_infinite(vec)
#> [1]  1  2  3 NA  5 NA NA
replace_infinite(vec, what = "NaN", value = NA)
#> [1]   1   2   3  NA   5  NA Inf
replace_infinite(vec, value = c(NA, 0))
#> [1]  1  2  3 NA  5 NA  0
```
