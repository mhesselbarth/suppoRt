# save_rds

Save rds

## Usage

``` r
save_rds(
  object,
  filename = NULL,
  path = NULL,
  overwrite = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  R object to serialize.

- filename:

  Name of the file where the R object is saved (file extension must be
  .rds).

- path:

  Path to where the R object is saved.

- overwrite:

  If true, existing file is overwritten.

- verbose:

  If true, messages are printed to the console.

- ...:

  Options passed to saveRDS.

## Details

This function checks if the file aready exists before saving it. If the
file aready exists and `overwrite = FALSE` (default), the file is not
saved. The filename and path is combined using
[`file.path()`](https://rdrr.io/r/base/file.path.html). See
`?base::saveRDS()` for more information.

It is possible to set `path = ""` and use the entire path including the
file name as `filename` argument.

## See also

[`saveRDS`](https://rdrr.io/r/base/readRDS.html)

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(42)
x <- runif(n = 100)
save_rds(x, filename = "random_numbers.rds")
} # }
```
