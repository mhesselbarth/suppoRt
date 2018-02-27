#' Function to save ggplot
#'
#' This function checks if the plot aready exists before saving it.
#' See ?ggplot2::ggsave() for more information
#' @param plot [\code{ggplot(1)}]\cr Plot to save
#' @param filename [\code{string(1)}]\cr File name to create on disk

#' @export
Save.Function.ggplot <- function(plot, filename, ...){
  if (file.exists(filename)){
    input <- utils::menu(title="Do you want to overwrite already existing file?", choices=c("Yes", "No"))
    if(input==1){
      ggplot2::ggsave(plot=plot, filename=filename, ...)
      base::print("Existing file overwriten")
    }
    else{base::print("File not saved")}

  }
  else{
    ggplot2::ggsave(plot=plot, filename=filename, ...)
    base::print("New file written")
  }
}
