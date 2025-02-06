#' zotero_linked_files
#'
#' @description List linked files
#'
#' @param lib_file A string with the path to the Zotero library exported as a CSV file.
#' @param full_name Logical if full path to the files or only the file names should be returned
#'
#' @details
#' This function reads a CSV file exported from Zotero and extracts the information about
#' the files linked to the references in the library.
#'
#' To export your library from Zotero, go to the menu `File > Export Library...`
#' and choose the CSV format.
#'
#' @references
#' Adapted from Daniel Vartanian (https://gist.github.com/danielvartan/924817b7e4b69212beb217f339c37a3f)
#'
#' @return vector
#'
#' @examples
#' \dontrun{zotero_linked_files()}
#'
#' @keywords internal
zotero_linked_files <- function(lib_file = file.choose(), full_name = TRUE) {

  # read file names of Zotero database
  file_vec <- utils::read.csv(lib_file, na.strings = c("NA", ""))[, "File.Attachments"]

  # clean paths
  file_list <- stringr::str_split(string = file_vec, pattern = "; (?=[A-Z]:)")

  # convert to vector
  file_vec <- unlist(file_list)

  # remove white spaces
  file_vec <- stringr::str_squish(string = file_vec)

  # remove something :)
  file_vec <- stringr::str_remove(string = file_vec, pattern = "[^A-Za-z0-9]$")

  # remove NAs
  out <- purrr::discard(file_vec, is.na)

  if (full_name) {basename(out)} else {out}
}



