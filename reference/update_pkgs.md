# update_pkgs

Update packages

## Usage

``` r
update_pkgs(exclude = NULL)
```

## Arguments

- exclude:

  Vector with package names not to update.

## Value

void

## Details

The function updates all installed packages using the `remotes` package.
It allows to exclude package (e.g., because only available in non-public
GitHub repo).

## Examples

``` r
if (FALSE) { # \dontrun{
update_pkgs(exclude = c("pkg_a", "pkg_b"))
} # }
```
