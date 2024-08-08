#' save_rds
#'
#' @description Save rds
#'
#' @param object R object to serialize.
#' @param filename Name of the file where the R object is saved (file extension must be .rds).
#' @param path Path to where the R object is saved.
#' @param overwrite If true, existing file is overwritten.
#' @param rm_object If true, the object is removed form the environment.
#' @param ... Options passed to saveRDS.
#'
#' @details
#' This function checks if the file aready exists before saving it. If the file aready
#' exists and \code{overwrite = FALSE} (default), the file is not saved. The filename and path
#' is combined using \code{file.path()}. See \code{?base::saveRDS()} for more information.
#'
#' It is possible to set \code{path = ""} and use the entire path including the file
#' name as \code{filename} argument.
#'
#' @seealso
#' \code{\link{saveRDS}}
#
#' @examples
#' \dontrun{
#' set.seed(42)
#' x <- runif(n = 100)
#' save_rds(x, filename = "random_numbers.rds")
#' }
#'
#' @export
save_rds <- function(object, filename = NULL, path = NULL,
                     overwrite = FALSE, rm_object = FALSE, ...){

  # object_name <- deparse(substitute(object))

  if (is.null(path)) {path <- getwd()}

  if (is.null(filename)) {filename <- "rds_file.rds"}

  complete_file <- file.path(path, filename)

  if (substr(x = complete_file, start = 1, stop = 1) == "/") {

    complete_file <- sub(pattern = "/", replacement = "", x = complete_file)

  }

  message("> Trying to save file: ", complete_file, appendLF = TRUE)

  if (base::file.exists(complete_file)) {

    if (overwrite == TRUE) {

      saveRDS(object = object, file = complete_file, ...)

      message("> Existing file overwriten", appendLF = TRUE)

      # if (rm_object) {
      #
      #   # remove object from environment
      #   remove(object_name, inherits = TRUE, envir = globalenv())
      # }
    }

    else{

      warning("Existing file not overwriten", call. = FALSE)
      }
  }

  else{
    saveRDS(object = object, file = complete_file, ...)

    message("> New file written", appendLF = TRUE)

    # if (rm_object) {
    #
    #   # remove object from environment
    #   remove(object_name, inherits = TRUE, envir = globalenv())
    # }
  }
}
