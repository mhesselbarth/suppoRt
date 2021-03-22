#' calc_cv
#'
#' @description Calculate coefficient of variation.
#'
#' @param x vector.
#' @param na.rm Logical if `NA` should also be removed
#'
#' @details
#' Function to calculate coefficient of variation cv = sd / mean * 100 of vector.
#'
#' @return numeric
#'
#' @examples
#' x <- runif(n = 100)
#' calc_cv(x = x)
#'
#' @aliases calc_cv
#' @rdname calc_cv

#' @export

calc_cv <- function(x, na.rm = FALSE) {

  m <- mean(x, na.rm = na.rm)

  s <- sd(x, na.rm = na.rm)

  cv <- (s / m) * 100

  return(cv)
}
