# show_environment

Show memory use

## Usage

``` r
show_environment(
  what = NULL,
  sort = "size",
  units = "Mb",
  decreasing = TRUE,
  n = NULL
)
```

## Arguments

- what:

  List with objects to show object size. If `NULL` all objects in
  environment are used.

- sort:

  Sort the resulting dataframe either by "size", "name".

- units:

  Unit used to format memory usage. For more details, see
  [`?object.size`](https://rdrr.io/r/utils/object.size.html).

- decreasing:

  Logical if results should be sorted in decreasing order.

- n:

  Number of top rows to print.

## Value

data.frame

## Details

Functions returns the name, class and size of all objects in the
environment. The resulting data frame can be sorted by different
arguments.

## See also

[`object.size`](https://rdrr.io/r/utils/object.size.html)

## Examples

``` r
vec <- 1:100
vec_2 <- runif(n = 10)
df <- data.frame(1:5000)
mat <- matrix(c(1:100), ncol = 20, nrow = 5)
list <- list(x = vec, y = df, z = mat)

show_environment()
#> Error in get(x): object 'mat' not found
```
