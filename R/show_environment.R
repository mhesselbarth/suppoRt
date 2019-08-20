#' show_environment
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
#' vec_2 <- runif(n = 10)
#' df <- data.frame(1:5000)
#' mat <- matrix(c(1:100), ncol = 20, nrow = 5)
#' list <- list(x = vec, y = df, z = mat)
#'
#' show_environment()
#'
#' @aliases show_environment
#' @rdname show_environment

#' @export
show_environment <- function(what = NULL, sort = "size", units = "Mb", decreasing = TRUE, n = NULL) {


  if (is.null(what)) {
    present_objects <- ls(parent.frame()) # get all present objects in environment

    what <- lapply(present_objects, function(x) get(x))
  }

  else {
    if (!is.null(names(what))) {
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
  information_objects <- lapply(what, function(x) {

    # get current size and split into numeric and unit
    current_size <- format(utils::object.size(x), units = units)
    current_size <- strsplit(current_size, split = " ")[[1]]

    # get class of object
    current_class <- class(x)[[1]]

    if (is.vector(x) & !is.list(x)) {
      length <- length(x)
      dimension <- c(NA, NA)
    }

    else if (is.data.frame(x) | is.matrix(x)) {
      length <- NA
      dimension <- dim(x)
    }

    else if (is.list(x)) {
      length <- length(x)
      dimension <- c(NA, NA)
    }

    else {
      length <- NA
      dimension <- c(NA, NA)
    }

    # combine to one df
    data.frame(class = as.character(current_class),
               size = as.numeric(current_size[[1]]),
               unit = current_size[[2]],
               length = length,
               nrow = dimension[[1]],
               ncol = dimension[[2]])
  })

  names(information_objects) <- present_objects

  # rowbind to one df and add names
  information_objects <- do.call(rbind, information_objects)

  row.names(information_objects) <- NULL

  information_objects <- cbind(name = present_objects, information_objects)

  # sort data
  if (sort == "name") {
    information_objects <- information_objects[order(information_objects$name, decreasing = decreasing), ]
  }

  else {
    information_objects <- information_objects[order(information_objects$size, decreasing = decreasing), ]

    if (sort != "size") {warning("sort argument unkown - using size",
                                call. = FALSE)
    }
  }

  # only print top n rows
  if (!is.null(n)) {
    information_objects <- information_objects[1:n, ]
  }

  # return as tibble if installed
  if (nzchar(system.file(package = "tibble"))) {
    information_objects <- tibble::as_tibble(information_objects)
  }

  return(information_objects)
}
