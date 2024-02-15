#' remove_tmp
#'
#' @description Remove temporary folders
#'
#' @param delete Logical if folders should be deleted
#'
#' @details
#' The functions searches for all temporary folders and returns them as a vector.
#' Can also delete all files using `unlink(x, recursive = TRUE, force = TRUE, expand = TRUE)`
#'
#' @return  vector
#'
#' @examples
#' remove_tmp()
#'
#' @export
remove_tmp <- function(delete = FALSE) {

  # get path to temp dir
  temp_dir <- Sys.getenv("TEMP")

  # detect and delete folders with pattern "rtmp"
  folders <- dir(temp_dir, pattern = "^Rtmp", full.names = TRUE)

  # delete folders
  if (delete) {
    unlink(folders, recursive = TRUE, force = TRUE, expand = TRUE)
  }

  return(folders)

}
