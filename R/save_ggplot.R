#' save_ggplot
#'
#' @description Save ggplot
#'
#' @param plot Plot to save.
#' @param filename File name to create on disk. The device (e.g. jpeg) must be specified as file extension.
#' @param path Path to save plot to (combined with filename).
#' @param overwrite If true, existing file is overwritten.
#' @param rm_plot If true, the plot is removed form the environment.

#' @param ... Options passed to ggsave.
#'
#' @details
#' This function checks if the plot aready exists before saving it. If the file aready
#' exists and \code{overwrite = FALSE} (default), the file is not saved. The filename and path
#' is combined using \code{file.path()}. See \code{?ggplot2::ggsave()} for more information.
#'
#' It is possible to set \code{path = ""} and use the entire path including the file
#' name as \code{filename} argument.
#'
#' @seealso
#' \code{\link{ggsave}}
#'
#' @examples
#' \dontrun{
#' x <- runif(n = 100)
#' y <- runif(n = 100)
#' result_plot <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x = x, y = y))
#' save_ggplot(result_plot, filename = "result_ggpplot.jpeg")
#' }
#'
#' @export
save_ggplot <- function(plot, filename = NULL, path = NULL,
                        overwrite = FALSE, rm_plot = FALSE, ...) {

  # plot_name <- deparse(substitute(object))

  if (is.null(path)) {path <- getwd()}

  if (is.null(filename)) {filename <- 'R_plot.jpeg'}

  complete_file <- file.path(path, filename)

  if (substr(x = complete_file, start = 1, stop = 1) == "/") {

    complete_file <- sub(pattern = "/", replacement = "", x = complete_file)

  }

  message("> Trying to save ggplot: ", complete_file, appendLF = TRUE)

  if (file.exists(complete_file)) {

    if (overwrite == TRUE) {

      ggplot2::ggsave(plot = plot, filename = filename, path = path, ...)

      message("> Existing ggplot overwriten", appendLF = TRUE)

      # if (rm_plot) {
      #
      #   # remove plot from environment
      #   remove(plot_name, envir = globalenv())
      # }
    }

    else{

      warning("Existing ggplot not overwriten", call. = FALSE)
      }
  }

  else{

    ggplot2::ggsave(plot = plot, filename = filename, path = path, ...)

    message("> New ggplot written", appendLF = TRUE)

    # if (rm_plot) {
    #
    #   # remove plot from environment
    #   remove(plot_name, envir = globalenv())
    # }
  }
}
