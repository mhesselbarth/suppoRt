#' remove_depends
#'
#' @description Remove dependencies
#'
#' @param pkg R object to serialize.
#' @param recursive Name of the file where the R object is saved (file extension must be .rds).
#'
#' @details
#' Function removes a package with all dependencies
#
#' @examples
#' \dontrun{
#' remove_depends("dplyr")
#' }
#'
#' @aliases remove_depends
#' @rdname remove_depends

#' @export
remove_depends <- function(pkg, recursive = FALSE){

  # get all dependencies
  dependencies <- tools::package_dependencies(pkg, utils::installed.packages(), recursive = recursive)

  # are dependencies present
  depends <- if (!is.null(dependencies[[pkg]])) dependencies[[pkg]] else character()

  # get dependencies still needed
  needed <- unique(unlist(dependencies[!names(dependencies) %in% c(pkg, depends)]))

  # packages that can be removed
  to_remove <- depends[!depends %in% needed]

  if (length(to_remove) > 0) {

    to_remove <- utils::select.list(choices = c(pkg, sort(to_remove)),
                                    multiple = TRUE,
                                    title = "Select packages to remove")

    utils::remove.packages(to_remove)
  }

  else {
    stop("Nothing to remove.", call. = FALSE)
  }
}
