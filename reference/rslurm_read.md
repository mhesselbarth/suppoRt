# rslurm_read

rslurm read

## Usage

``` r
rslurm_read(x, pattern, as_list = TRUE)
```

## Arguments

- x:

  slurm_job object.

- pattern:

  String with pattern to match.

- as_list:

  Logical if result should be returned as `list` or `data.frame`.

## Value

list

## Details

This functions reads all `.rds` files matching the `pattern` argument.
This can be useful if results were not saved using the default templates
of the `rslurm` package. For more information, please see the `rslurm`
package.

## Examples

``` r
if (FALSE) { # \dontrun{
rslurm_read(x = sbatch_results)
} # }
```
