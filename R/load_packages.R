#' load_packages
#'
#' @description Load pacakges
#'
#' @param packages String with package names
#' @param install If "yes" missing packages are automatically installed. If "no" only already
#' installed packages are loaded. If "ask", an user input is needed.
#' @param verbose Print messages
#'
#'
#' @details
#' The function loads libraries. It's possible to provide a vector with string.
#' Missing packages can be installed automatically
#'
#' @return NA
#'
#' @examples
#' \dontrun{
#' load_packages(packages = c("dplyr", "spatstat"))
#' }
#'
#' @aliases load_packages
#' @rdname load_packages

#' @export
load_packages <- function(packages, install = "ask", verbose = TRUE) {

  # set selection to 0 as default
  selection <- 0

  # check which packages are installed
  result <- vapply(packages, function(x) nzchar(system.file(package = x)),
                   FUN.VALUE = logical(1))

  # all packages are installed
  if (all(result)) {

    for (i in seq_along(packages)) {

      suppressMessages(library(package = packages[[i]],
                               character.only = TRUE,
                               quietly = TRUE, verbose = FALSE,
                               warn.conflicts = FALSE))

      # print if library is loaded
      if (verbose) {

        if (packages[[i]] %in% .packages()) {

          message("> '",  packages[[i]], "' is loaded")
        }

        else {

          message("> '",  packages[[i]], "' NOT loaded")
        }
      }
    }
  }

  else {

    # print which pacakges are not installed
    if (verbose) {

      message("> Not all packages installed.")

      data.frame(package = packages,
                 installed = result,
                 row.names = NULL)
    }

    # do not install packages
    if (install == "no") {

      if (verbose) {

        message("> Missing packages not installed.")
      }

      already_installed <- packages[result]

      for (i in seq_along(already_installed)) {

        suppressMessages(library(package = already_installed[[i]],
                                 character.only = TRUE,
                                 quietly = TRUE, verbose = FALSE,
                                 warn.conflicts = FALSE))

        # print if library is loaded
        if (verbose) {

          if (already_installed[[i]] %in% .packages()) {

            message("> '",  already_installed[[i]], "' is loaded")
          }

          else {

            message("> '",  already_installed[[i]], "' NOT loaded")
          }
        }
      }
    }

    # ask if packages should be installed
    else if (install == "ask") {

      selection <- utils::menu(title = "Install missing packages?",
                               choices = c("Yes", "No"))
    }

    # install packages if install == yes or selection == 1
    if (install == "yes" | selection == 1) {

      # missing packages
      to_install <- packages[!result]

      # install missing packages
      for (i in seq_along(to_install)) {

        suppressMessages(utils::install.packages(to_install[[i]],
                                                 verbose = FALSE,
                                                 quiet = TRUE))
      }

      for (i in seq_along(packages)) {

        suppressMessages(library(package = packages[[i]],
                                 character.only = TRUE,
                                 quietly = TRUE, verbose = FALSE,
                                 warn.conflicts = FALSE))

        # print if library is loaded
        if (verbose) {

          if (packages[[i]] %in% .packages()) {

            message("> '",  packages[[i]], "' is loaded")
          }

          else {

            message("> '",  packages[[i]], "' NOT loaded")
          }
        }
      }
    }

    # wrong option for install
    else {
      stop("'install must be 'ask', 'yes', or 'no'.", call. = FALSE)
    }
  }
}
