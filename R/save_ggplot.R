#' Function to save ggplot
#'
#' This function checks if the plot aready exists before saving it.
#' See ?ggplot2::ggsave() for more information
#' @param plot [\code{ggplot(1)}]\cr Plot to save
#' @param filename [\code{string(1)}]\cr File name to create on disk. The device (e.g. jpeg) must be specified as file extension
#' @param path [\code{string(1)}]\cr Path to save plot to (combined with filename)
#' @param overwrite [\code{logical(1)}]\cr Overwrite if the plot alreay exists

#' @export
save_ggplot <- function(plot, filename = NULL, path = NULL, overwrite = FALSE, ...) {

  if(is.null(path)){path <- getwd()}
  if(is.null(filename)){filename <- 'R_plot.jpeg'}

  complete_file <- base::file.path(path, filename)
  cat(paste0("Trying to save ggplot:\n", complete_file, "\n\n"))

  if (base::file.exists(complete_file)){
    if(overwrite == TRUE){

      ggplot2::ggsave(plot = plot, filename = filename, path = path, ...)
      cat("Existing ggplot overwriten \n")
    }

    else{cat("ggplot not saved \n")}
  }

  else{

    ggplot2::ggsave(plot=plot, filename=filename, path=path, ...)
    base::cat("New ggplot written \n")
  }
}
