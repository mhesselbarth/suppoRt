#' show_temp
#'
#' @description Remove temporary folders
#'
#' @param remove Logical if folders should be deleted.
#' @param verbose Logical if messages should be printed.
#'
#' @details
#' The functions searches for all temporary folders and returns them as a vector.
#' Can also delete all files using `unlink(x, recursive = TRUE, force = TRUE, expand = TRUE)`
#'
#' @return  vector
#'
#' @examples show_temp()
#'
#' @export
show_temp <- function(remove = FALSE, verbose = TRUE) {

  # get path to temp dir
  temp_dir <- Sys.getenv("TEMP")

  # detect folders with pattern "rtmp"
  folders <- dir(temp_dir, pattern = "^Rtmp", full.names = TRUE)

  # delete folders
  if (remove) {

    unlink(folders, recursive = TRUE, force = TRUE, expand = TRUE)

    if (verbose) message("Deleted ", length(folders), " temporary files.")

  }

  return(folders)

}
