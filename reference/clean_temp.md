# Delete temporary files

Function to delete temporary files

## Usage

``` r
clean_temp(
  base_dir = dirname(tempdir()),
  minutes_keep = 0,
  list_files = FALSE,
  delete = FALSE,
  verbose = getOption("energyTools.verbose", TRUE)
)
```

## Arguments

- base_dir:

  Character with path to temp files.

- minutes_keep:

  Numeric with threshold in minutes for files to keep.

- list_files:

  Logical if all files should be listed.

- delete:

  Logical if all files should be deleted.

- verbose:

  Logical if messages are send to console.

## Value

(invisible) list

## Details

The function deletes all files in the temporary folder which are older
than `minutes_keep`.

## Author

Code based on scripts of Markus Samek

## Examples

``` r
if (FALSE) { # \dontrun{
clean_temp()
} # }
```
