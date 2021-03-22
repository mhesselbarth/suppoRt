#' as_data_table
#'
#' @description Convert raster object to data.table
#'
#' @param x RasterLayer.
#' @param na.rm Logical if `NA` should be removed.
#' @param return_df If TRUE, `data.frame` instead of `data.table` is returned.
#' @param verbose If TRUE, progress report is printed.
#'
#' @details
#' Function to convert a RasterLayer into a data.table. For rasters that cannot be
#' hold in memory, values are written blockwise.
#'
#' @references
#' Adapted from Etienne B. Racine (https://gist.github.com/etiennebr/9515738)
#'
#' @return data.table
#'
#' @example
#' ras <- raster::raster(nrow = 10, ncol = 10)
#' raster::values(ras) <- sample(x = c(1, 2, 3), size = 100, replace = TRUE)
#' as_data_table(x = ras)
#'
#' @aliases as_data_table
#' @rdname as_data_table
#'
#' @export
as_data_table <- function(x, na.rm = TRUE, return_df = TRUE, verbose = TRUE) {

  # create column names
  col_names <- paste0("layer_", 1:raster::nlayers(x))

  # get block size
  blcks <- raster::blockSize(x)

  result_temp <- lapply(1:blcks$n, function(i) {

    # this just prints a progress report
    if (verbose) {

      message("\r> Progress: ", i, " / ", blcks$n, "\t\t", appendLF = FALSE)

    }

    # get values of current chunk
    data_temp <- data.table::data.table(raster::getValuesBlock(x,
                                                               row = blcks$row[i],
                                                               nrows = blcks$nrows[i]))

    # get cell ids of current chunkg
    cells <- raster::cellFromRowCol(object = x,
                                    row = c(blcks$row[i], blcks$row[i] + blcks$nrows[i] - 1),
                                    col = c(1, raster::ncol(x)))

    # get xy coords of current chunk
    data_temp[, c("x", "y") := data.table::data.table(raster::xyFromCell(x, cell = cells[1]:cells[2]))]

    names(data_temp) <- c(col_names, "x", "y")

    # make sure coll order is in correct order
    data.table::setcolorder(data_temp, c("x", "y", col_names))

    # remove all rows in which all layers are NA
    if (na.rm) {

      data_temp <- stats::na.omit(data_temp, cols = col_names)

    }

    return(data_temp)
  })

  message("\n> Combine results to one data.table")

  # combine list of data.table to one data.table
  result_temp <- data.table::rbindlist(result_temp)

  # convert to data.frame
  if (return_df) {

    result_temp <- as.data.frame(result_temp)

  }

  return(result_temp)
}
