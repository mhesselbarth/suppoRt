# Delete temporary files

Delete temporary files

## Usage

``` r
clean_temp(
  base_dir = dirname(tempdir()),
  minutes_keep = 0,
  list_files = FALSE,
  delete = FALSE,
  verbose = TRUE
)
```

## Arguments

- base_dir:

  Character string with the path to the temporary directory.

- minutes_keep:

  Numeric. Age in minutes from which on folders are deleted; more recent
  folders are kept.

- list_files:

  Logical, whether all files should be listed.

- delete:

  Logical, weather all files should be deleted.

- verbose:

  Logical, whether messages are sent to the console.

## Value

(invisible) list

## Details

The function deletes all files in the temporary folder which are older
than `minutes_keep`.

## Author

Code based on scripts by Markus Samek

## Examples

``` r
if (FALSE) { # \dontrun{
clean_temp()
} # }
```
