# expand_grid_unique

Expand grid unique

## Usage

``` r
expand_grid_unique(x, y, equals = FALSE)
```

## Arguments

- x:

  vector x.

- y:

  vector y

- equals:

  If TRUE also same combinations are included

## Value

vector

## Details

The function returns all possible combinations of two vectors.

## Examples

``` r
x <- 1:5
y <- 1:5
expand_grid_unique(x, y)
#>       [,1] [,2]
#>  [1,]    1    2
#>  [2,]    1    3
#>  [3,]    1    4
#>  [4,]    1    5
#>  [5,]    2    3
#>  [6,]    2    4
#>  [7,]    2    5
#>  [8,]    3    4
#>  [9,]    3    5
#> [10,]    4    5
```
