#' expand_grid_unique
#'
#' @description Expand grid unique
#'
#' @param x vector x.
#' @param y vector y
#' @param equals If TRUE also same combinations are included
#'
#' @details
#' The function returns all possible combinations of two vectors.
#'
#' @return vector
#'
#' @examples
#' x <- 1:5
#' y <- 1:5
#' expand_grid_unique(x, y)
#'
#' @aliases expand_grid_unique
#' @rdname expand_grid_unique

#' @export
expand_grid_unique <- function(x, y, equals = FALSE)
{
  x <- unique(x)

  y <- unique(y)

  g <- function(i) {
    z <- setdiff(y, x[seq_len(i - equals)])

    if (length(z)) cbind(x[i], z, deparse.level = 0)
  }

  do.call(rbind, lapply(seq_along(x), g))
}
