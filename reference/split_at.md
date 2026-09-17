# split_at

Split vector

## Usage

``` r
split_at(x, pos)
```

## Arguments

- x:

  Vector.

- pos:

  Vector with positions to split.

## Value

list

## Details

Split vector at position(s).

## Examples

``` r
x <- c(1, 2, 3, 1, 5, 3, 1)
split_at(x, pos = c(2, 5))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2 3 1
#> 
#> [[3]]
#> [1] 5 3 1
#> 
```
