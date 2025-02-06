#' zotero_missing_files
#'
#' @description Find missing PDF files
#'
#' @param lib_file A string with the path to the Zotero library exported as a CSV file.
#' @param file_folder A string with the path to the folder containing the files
#' linked to the references in the Zotero library.
#'
#' @details
#' This function compares the files in a folder with the files linked to the
#' references in a Zotero library and returns the names of the missing PDF files.
#'
#' @references
#' Adapted from Daniel Vartanian (https://gist.github.com/danielvartan/924817b7e4b69212beb217f339c37a3f)
#'
#' @return vector
#'
#' @examples
#' \dontrun{zotero_missing_files()}
#'
#' @export
zotero_missing_files <- function(lib_file = file.choose(), file_folder = rstudioapi::selectDirectory()) {

  # list all linked files
  linked_files <- zotero_linked_files(lib_file, full_name = TRUE)

  # list all present files
  real_files <- basename(list.files(file_folder))

  # get all files without an entry
  missing <- linked_files[!linked_files %in% real_files]

  # return
  return(missing)

}

