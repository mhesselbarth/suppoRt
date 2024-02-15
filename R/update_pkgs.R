#' update_pkgs
#'
#' @description Update packages
#'
#' @param exclude Vector with package names not to update.
#'
#' @details
#' The function updates all installed packages using the \code{remotes} package.
#' It allows to exclude package (e.g., because only available in non-public GitHub repo).
#'
#' @return void
#'
#' @examples
#' \dontrun{
#' update_pkgs(exclude = c("pkg_a", "pkg_b"))
#' }
#'
#' @export
update_pkgs <- function(exclude = NULL) {

  # get all installed packages
  installed <- rownames(utils::installed.packages())

  # remove exclude packages
  updatable <- setdiff(installed, exclude)

  # run update function
  remotes::update_packages(packages = updatable)

}
