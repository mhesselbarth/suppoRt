# save_ggplot

Save ggplot

## Usage

``` r
save_ggplot(
  plot,
  filename = NULL,
  path = NULL,
  overwrite = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- plot:

  Plot to save.

- filename:

  File name to create on disk. The device (e.g. jpeg) must be specified
  as file extension.

- path:

  Path to save plot to (combined with filename).

- overwrite:

  If true, existing file is overwritten.

- verbose:

  If true, messages are printed to the console.

- ...:

  Options passed to ggsave.

## Details

This function checks if the plot aready exists before saving it. If the
file aready exists and `overwrite = FALSE` (default), the file is not
saved. The filename and path is combined using
[`file.path()`](https://rdrr.io/r/base/file.path.html). See
`?ggplot2::ggsave()` for more information.

It is possible to set `path = ""` and use the entire path including the
file name as `filename` argument.

## See also

[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- runif(n = 100)
y <- runif(n = 100)
result_plot <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x = x, y = y))
save_ggplot(result_plot, filename = "result_ggpplot.jpeg")
} # }
```
