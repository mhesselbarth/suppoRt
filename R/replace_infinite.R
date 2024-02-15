#' replace_infinite
#'
#' @description Replace infinite
#'
#' @param x vector.
#' @param what What infinite values should be replaced. See details.
#' @param value Value that will be used to replace selected infinite values.
#' @param verbose Print warning messages.
#'
#' @details
#' The function converts all `NaN` and `Inf` values to NA. Input must be a vector.
#' What can be either `what = c("NaN", "Inf")` to replace both `NaN` and `Inf` or just
#' one of the two, e.g. `what = "NaN`.
#'
#' @return  vector
#'
#' @examples
#' vec <- c(1, 2, 3, NA, 5, NaN, Inf)
#' replace_infinite(vec)
#' replace_infinite(vec, what = "NaN", value = NA)
#' replace_infinite(vec, value = c(NA, 0))
#'
#' @export
replace_infinite <- function(x, what = c("NaN", "Inf"), value = c(NA, NA),
                             verbose = TRUE) {

  # check if input is vector
  if (!is.vector(x)) {
    stop("input must be a vector.", call. = FALSE)
  }

  # check if input is numeric
  if (!inherits(x = x, what = "numeric")) {
    stop("input must be of class 'numeric'.", call. = FALSE)
  }

  # replace NaN and Inf
  if (all(what == c("NaN", "Inf"))) {

    if (length(value) < 2) {
      if (verbose) {
        warning("Using 'value' as replace value twice", call. = FALSE)
      }

      value <- c(value, value)
    }

    x <- ifelse(test = is.nan(x), yes = as.numeric(value[[1]]), no = x)

    x <- ifelse(test = is.infinite(x), yes = as.numeric(value[[2]]), no = x)

    return(x)
  }

  # only replay NaN
  else if (what == "NaN") {

    if (length(value) > 1) {
      warning("Only using first 'value' as replace value", call. = FALSE)
    }

    x <- ifelse(test = is.nan(x), yes = as.numeric(value[[1]]), no = x)

    return(x)
  }

  # only replace Inf
  else if (what == "Inf") {

    if (verbose) {
      if (length(value) > 1) {
        warning("Only using first 'value' as replace value", call. = FALSE)
      }
    }

    x <- ifelse(test = is.infinite(x), yes = as.numeric(value[[1]]), no = x)

    return(x)
  }

  # return error
  else {
    stop("'what' must be 'NaN', 'Inf' or 'c(NaN, Inf)'.", call. = FALSE)
  }
}

