#' save_ggplot
#'
#' @description Save ggplot
#'
#' @param plot Plot to save.
#' @param filename File name to create on disk. The device (e.g. jpeg) must be specified as file extension.
#' @param path Path to save plot to (combined with filename).
#' @param overwrite If true, existing file is overwritten.
#' @param ... Options passed to ggsave.
#'
#' @details
#' This function checks if the plot aready exists before saving it. If the file aready
#' exists and `overwrite = FALSE` (default), the file is not saved. The filename and path
#' is combined using `file.path()`. See `?ggplot2::ggsave()` for more information.
#'
#' @seealso
#' \code{\link{ggsave}}
#'
#' @examples
#' set.seed(42)
#' pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)
#' csr_envelope <- spatstat::envelope(pattern, fun = spatstat::pcf, nsim = 39)
#' result_plot <- quantum_plot(csr_envelope, ylab = "g(r)")
#' save_ggplot(result_plot, filename = "result_ggpplot.jpeg")
#'
#' @aliases save_ggplot
#' @rdname save_ggplot

#' @export
save_ggplot <- function(plot, filename = NULL, path = NULL, overwrite = FALSE, ...) {

  if(is.null(path)){path <- getwd()}
  if(is.null(filename)){filename <- 'R_plot.jpeg'}

  complete_file <- file.path(path, filename)
  cat(paste0("Trying to save ggplot:\n", complete_file, "\n\n"))

  if (file.exists(complete_file)){
    if(overwrite == TRUE){

      ggplot2::ggsave(plot = plot, filename = filename, path = path, ...)
      cat("Existing ggplot overwriten \n")
    }

    else{cat("ggplot not saved \n")}
  }

  else{

    ggplot2::ggsave(plot=plot, filename=filename, path=path, ...)
    cat("New ggplot written \n")
  }
}
