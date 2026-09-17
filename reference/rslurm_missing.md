# rslurm_missing

rslurm missing

## Usage

``` r
rslurm_missing(x)
```

## Arguments

- x:

  slurm_job object.

## Value

vector

## Details

The function checks if all .RDS files are present in the `rslurm_job`
project. For more information, please see the `rslurm` package.

## Examples

``` r
if (FALSE) { # \dontrun{
rslurm_missing(x = sbatch_results)
} # }
```
