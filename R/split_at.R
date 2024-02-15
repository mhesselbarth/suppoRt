#' split_at
#'
#' @description Split vector
#'
#' @param x Vector.
#' @param pos Vector with positions to split.
#'
#' @details
#' Split vector at position(s).
#'
#' @examples
#' x <- c(1, 2, 3, 1, 5, 3, 1)
#' split_at(x, pos = c(2, 5))
#'
#' @return list
#'
#' @export
split_at <- function(x, pos) {

  unname(split(x, cumsum(seq_along(x) %in% pos)))

}
