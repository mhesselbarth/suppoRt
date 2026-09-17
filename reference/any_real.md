# any_real

Any real

## Usage

``` r
any_real(x, include_na = FALSE, verbose = TRUE)
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

The function checks if a vector contains `NaN` or `Inf` values. Input
must be a vector. If `include_na = TRUE` also `NA` values are
considered.

## Examples

``` r
x <- c(1, 2, 3, NA, 5, NaN, Inf)
any_real(x)
#> > Includes 2 non-real values
#> [1] TRUE

y <- c(1, 2, 3, 4, 5, 6, 7)
any_real(y)
#> > Includes 0 non-real values
#> [1] TRUE
```
