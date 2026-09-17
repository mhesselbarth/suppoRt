# as_data_table

Convert raster object to data.table

## Usage

``` r
as_data_table(x, na.rm = TRUE, return_df = TRUE, verbose = TRUE)
```

## Arguments

- x:

  RasterLayer.

- na.rm:

  Logical if \`NA\` should be removed.

- return_df:

  If TRUE, \`data.frame\` instead of \`data.table\` is returned.

- verbose:

  If TRUE, progress report is printed.

## Value

data.table

## Details

Function to convert a RasterLayer into a data.table. For rasters that
cannot be hold in memory, values are written blockwise.

## References

Adapted from Etienne B. Racine
(https://gist.github.com/etiennebr/9515738)

## Examples

``` r
ras <- raster::raster(nrow = 10, ncol = 10)
raster::values(ras) <- sample(x = c(1, 2, 3), size = 100, replace = TRUE)
as_data_table(x = ras)
#> > Progress: 1 / 4     
#> > Progress: 2 / 4     
#> > Progress: 3 / 4     
#> > Progress: 4 / 4     
#> 
#> > Combine results to one data.table
#>        x   y layer_1
#> 1   -162  81       1
#> 2   -126  81       3
#> 3    -90  81       3
#> 4    -54  81       1
#> 5    -18  81       2
#> 6     18  81       3
#> 7     54  81       3
#> 8     90  81       3
#> 9    126  81       2
#> 10   162  81       1
#> 11  -162  63       1
#> 12  -126  63       1
#> 13   -90  63       3
#> 14   -54  63       3
#> 15   -18  63       1
#> 16    18  63       2
#> 17    54  63       2
#> 18    90  63       3
#> 19   126  63       3
#> 20   162  63       2
#> 21  -162  45       3
#> 22  -126  45       1
#> 23   -90  45       1
#> 24   -54  45       1
#> 25   -18  45       2
#> 26    18  45       2
#> 27    54  45       3
#> 28    90  45       3
#> 29   126  45       2
#> 30   162  45       3
#> 31  -162  27       1
#> 32  -126  27       2
#> 33   -90  27       3
#> 34   -54  27       3
#> 35   -18  27       1
#> 36    18  27       2
#> 37    54  27       1
#> 38    90  27       2
#> 39   126  27       3
#> 40   162  27       2
#> 41  -162   9       1
#> 42  -126   9       2
#> 43   -90   9       1
#> 44   -54   9       2
#> 45   -18   9       3
#> 46    18   9       1
#> 47    54   9       2
#> 48    90   9       2
#> 49   126   9       3
#> 50   162   9       2
#> 51  -162  -9       1
#> 52  -126  -9       1
#> 53   -90  -9       3
#> 54   -54  -9       2
#> 55   -18  -9       1
#> 56    18  -9       1
#> 57    54  -9       3
#> 58    90  -9       3
#> 59   126  -9       2
#> 60   162  -9       3
#> 61  -162 -27       2
#> 62  -126 -27       3
#> 63   -90 -27       2
#> 64   -54 -27       3
#> 65   -18 -27       2
#> 66    18 -27       2
#> 67    54 -27       1
#> 68    90 -27       3
#> 69   126 -27       2
#> 70   162 -27       2
#> 71  -162 -45       3
#> 72  -126 -45       1
#> 73   -90 -45       2
#> 74   -54 -45       1
#> 75   -18 -45       1
#> 76    18 -45       3
#> 77    54 -45       2
#> 78    90 -45       1
#> 79   126 -45       2
#> 80   162 -45       2
#> 81  -162 -63       2
#> 82  -126 -63       3
#> 83   -90 -63       2
#> 84   -54 -63       2
#> 85   -18 -63       2
#> 86    18 -63       1
#> 87    54 -63       3
#> 88    90 -63       3
#> 89   126 -63       1
#> 90   162 -63       1
#> 91  -162 -81       1
#> 92  -126 -81       1
#> 93   -90 -81       2
#> 94   -54 -81       1
#> 95   -18 -81       1
#> 96    18 -81       1
#> 97    54 -81       2
#> 98    90 -81       1
#> 99   126 -81       3
#> 100  162 -81       3
```
