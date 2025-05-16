#' show_temp
#'
#' @description Remove temporary folders
#'
#' @param global Logical how path for temp files is searched.
#' @param remove Logical if folders should be deleted.
#' @param verbose Logical if messages should be printed.
#'
#' @details
#' The functions searches for all temporary folders and returns them as a vector.
#' Can also delete all files using \code{unlink(x, recursive = TRUE, force = TRUE, expand = TRUE)}
#'
#' @return  vector
#'
#' @examples show_temp()
#'
#' @export
show_temp <- function(global = TRUE, remove = FALSE, verbose = TRUE) {

  # get path to temp dir
  if (global) {
  
    temp_dir <- Sys.getenv("TMPDIR")
  
  } else {

    temp_dir <- tempdir()
    
  }

  # detect folders with pattern "rtmp"
  folders <- dir(temp_dir, pattern = "^Rtmp", full.names = TRUE)

  # delete folders
  if (remove) {

    unlink(folders, recursive = TRUE, force = TRUE, expand = TRUE)

    if (verbose) message("Deleted ", length(folders), " temporary file(s).")

  } else {
    return(folders)
  }
}
