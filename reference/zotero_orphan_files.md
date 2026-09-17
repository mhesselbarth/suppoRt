# zotero_orphan_files

Find orphan files

## Usage

``` r
zotero_orphan_files(
  lib_file = file.choose(),
  file_folder = rstudioapi::selectDirectory()
)
```

## Arguments

- lib_file:

  A string with the path to the Zotero library exported as a CSV file.

- file_folder:

  A string with the path to the folder containing the files linked to
  the references in the Zotero library.

## Value

vector

## Details

This function compares the files in a folder with the files linked to
the references in a Zotero library and returns the names of the orphan
files.

## References

Adapted from Daniel Vartanian
(https://gist.github.com/danielvartan/924817b7e4b69212beb217f339c37a3f)

## Examples

``` r
if (FALSE) zotero_orphan_files() # \dontrun{}
```
