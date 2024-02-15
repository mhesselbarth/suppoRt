#' all_real
#'
#' @description All real
#'
#' @param x vector.
#' @param include_na Logical if `NA` should also be considered as non-real.
#' @param verbose Logical if number of non-real numbers should be printed as message.
#'
#' @details
#' The function checks if a vector contains `NaN` or `Inf` values. Input must be a vector.
#' If `include_na = TRUE` also `NA`s are considered.
#'
#' @return  vector
#'
#' @examples
#' x <- c(1, 2, 3, NA, 5, NaN, Inf)
#' all_real(x)
#'
#' y <- c(1, 2, 3, 4, 5, 6, 7)
#' all_real(y)
#'
#' @export
all_real <- function(x, include_na = FALSE, verbose = TRUE) {

  # check if input is vector
  if (!is.vector(x)) {
    stop("input must be a vector.", call. = FALSE)
  }

  # check if input is numeric
  if (!inherits(x = x, what = "numeric")) {
    stop("input must be of class 'numeric'.", call. = FALSE)
  }

  if (!include_na) {
    result <- is.infinite(x) | is.nan(x)
  } else {
    result <- is.infinite(x) | is.nan(x) | is.na(x)
  }

  if (verbose) {
    message("> Includes ", sum(result), " non-real values")
  }

  result <- all(!result)

  return(result)
}
