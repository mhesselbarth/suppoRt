#' show_memory_use
#'
#' @description Show memory use
#'
#' @param sort Sort the resulting dataframe either by "size", "alphabetical" or "class".
#' @param units Unit used to format memory usage. For more details, see \code{?object.size}.
#' @param decreasing Logical if results should be sorted in decreasing order.
#'
#' @details
#' Functions returns the name, class and size of all objects in the environment. The
#' resulting data frame can be sorted by different arguments.
#'
#' @seealso
#' \code{\link{object.size}}
#
#' @examples
#' vec <- 1:100
#' df <- data.frame(1:5000)
#' mat <- matrix(c(1:100), ncol = 10)
#'
#' show_memory_use()
#'
#' @aliases show_memory_use
#' @rdname show_memory_use

#' @export
show_memory_use <- function(sort = "size", units = "Kb", decreasing = FALSE) {

  present_objects <- ls(parent.frame())

  if (length(present_objects) == 0) {
    stop("No objects in environment.", call. = FALSE)
  }

  memory_usage <- lapply(present_objects, function(x) {

    current_size <- format(utils::object.size(x), units = units)
    current_size <- strsplit(current_size, split = " ")[[1]]

    current_class <- class(get(x))

    data.frame(class = as.character(current_class),
               size = as.numeric(current_size[[1]]),
               unit = current_size[[2]])
  })

  memory_usage <- cbind(name = present_objects, do.call(rbind, memory_usage))

  if (sort == "alphabetical") {
    memory_usage <- memory_usage[order(memory_usage$name, decreasing = decreasing), ]
  }

  else if (sort == "class") {
    memory_usage <- memory_usage[order(memory_usage$class, decreasing = decreasing), ]
  }

  else {
    memory_usage <- memory_usage[order(memory_usage$size, decreasing = decreasing), ]

    if(sort != "size") {warning("sort argument unkown - using size",
                                call. = FALSE)
    }
  }

  return(memory_usage)
}
