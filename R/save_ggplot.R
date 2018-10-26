#' save_ggplot
#'
#' @details
#' This function checks if the plot aready exists before saving it.
#' See ?ggplot2::ggsave() for more information.
#'
#' @param plot Plot to save
#' @param filename File name to create on disk. The device (e.g. jpeg) must be specified as file extension
#' @param path Path to save plot to (combined with filename)
#' @param overwrite Overwrite if the plot alreay exists
#' @param ... Options passed to ggsave

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
