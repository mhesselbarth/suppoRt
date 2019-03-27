#' save_rds
#'
#' @description Save rds
#'
#' @param object R object to serialize.
#' @param filename Name of the file where the R object is saved (file extension must be .rds).
#' @param path Path to where the R object is saved.
#' @param overwrite If true, existing file is overwritten.
#' @param ... Options passed to saveRDS.
#'
#' @details
#' This function checks if the file aready exists before saving it. If the file aready
#' exists and `overwrite = FALSE` (default), the file is not saved. The filename and path
#' is combined using `file.path()`. See `?base::saveRDS()`` for more information
#'
#' @seealso
#' \code{\link{saveRDS}}
#
#' @examples
#' set.seed(42)
#' x <- runif(n = 100)
#' save_rds(x, filename = "random_numbers.rds")
#'
#' @aliases save_rds
#' @rdname save_rds

#' @export
save_rds <- function(object, filename = NULL, path = NULL, overwrite = FALSE, ...){

  if(is.null(path)) {path <- getwd()}

  if(is.null(filename)) {filename <- "rds_file.rds"}

  complete_file <- file.path(path, filename)

  message("> Trying to save file: ", complete_file, appendLF = TRUE)

  if (base::file.exists(complete_file)) {

    if(overwrite == TRUE){

      saveRDS(object = object, file = complete_file, ...)

      message("> Existing file overwriten", appendLF = TRUE)
    }
    else{stop("Existing file not overwriten", call. = FALSE)}
  }

  else{
    saveRDS(object = object, file = complete_file, ...)

    message("> New file written", appendLF = TRUE)
  }
}
