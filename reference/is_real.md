# is_real

Is real

## Usage

``` r
is_real(x, include_na = FALSE, verbose = TRUE)
```

## Arguments

- x:

  vector.

- include_na:

  Logical if \`NA\` should also be considered as non-real.

- verbose:

  Logical if number of non-real numbers should be printed as message.

## Value

vector

## Details

The function checks if a vector contains real numbers and returns
`FALSE` for `NaN` or `Inf` values. Input must be a vector. If
`include_na = TRUE` also `NA` values are considered.

## Examples

``` r
x <- c(1, 2, 3, NA, 5, NaN, Inf)
is_real(x)
#> > Includes 2 non-real values
#> [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
is_real(x, include_na = TRUE)
#> > Includes 3 non-real values
#> [1]  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE

y <- c(1, 2, 3, 4, 5, 6, 7)
is_real(y)
#> > Includes 0 non-real values
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```
