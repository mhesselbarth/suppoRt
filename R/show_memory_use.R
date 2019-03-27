#' show_memory_use
#'
#' @description Show memory use
#'
#' @param what List with objects to show object size. If \code{NULL} all objects in environment are used.
#' @param sort Sort the resulting dataframe either by "size", "name".
#' @param units Unit used to format memory usage. For more details, see \code{?object.size}.
#' @param decreasing Logical if results should be sorted in decreasing order.
#' @param n Number of top rows to print.
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
show_memory_use <- function(what = NULL, sort = "size", units = "Kb", decreasing = TRUE, n = NULL) {


  if(is.null(what)) {
    present_objects <- ls(parent.frame()) # get all present objects in environment

    what <- lapply(present_objects, function(x) get(x))
  }

  else {
    if(!is.null(names(what))) {
      present_objects <- names(what)
    }

    else {
      present_objects <- paste0("object_0", 1:length(what))
    }
  }

  # stop if environment is empty
  if (length(what) == 0) {
    stop("No objects in environment.", call. = FALSE)
  }

  # get memory usage and class
  memory_usage <- lapply(what, function(x) {

    # get current size and split into numeric and unit
    current_size <- format(utils::object.size(x), units = units)
    current_size <- strsplit(current_size, split = " ")[[1]]

    # get class of object
    current_class <- class(x)[[1]]

    # combine to one df
    data.frame(class = as.character(current_class),
               size = as.numeric(current_size[[1]]),
               unit = current_size[[2]])
  })

  names(memory_usage) <- present_objects

  # rowbind to one df and add names
  memory_usage <- do.call(rbind, memory_usage)

  row.names(memory_usage) <- NULL

  memory_usage <- cbind(name = present_objects, memory_usage)

  # sort data
  if (sort == "name") {
    memory_usage <- memory_usage[order(memory_usage$name, decreasing = decreasing), ]
  }

  else {
    memory_usage <- memory_usage[order(memory_usage$size, decreasing = decreasing), ]

    if(sort != "size") {warning("sort argument unkown - using size",
                                call. = FALSE)
    }
  }

  # only print top n rows
  if(!is.null(n)) {
    memory_usage <- memory_usage[1:n, ]
  }

  return(memory_usage)
}
