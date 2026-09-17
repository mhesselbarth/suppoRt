# zotero_linked_files

List linked files

## Usage

``` r
zotero_linked_files(lib_file = file.choose(), full_name = TRUE)
```

## Arguments

- lib_file:

  A string with the path to the Zotero library exported as a CSV file.

- full_name:

  Logical if full path to the files or only the file names should be
  returned

## Value

vector

## Details

This function reads a CSV file exported from Zotero and extracts the
information about the files linked to the references in the library.

To export your library from Zotero, go to the menu \`File \> Export
Library...\` and choose the CSV format.

## References

Adapted from Daniel Vartanian
(https://gist.github.com/danielvartan/924817b7e4b69212beb217f339c37a3f)

## Examples

``` r
if (FALSE) zotero_linked_files() # \dontrun{}
```
