#' Function to save ggplot
#'
#' This function checks if the plot aready exists before saving it.
#' See ?ggplot2::ggsave() for more information
#' @param plot [\code{ggplot(1)}]\cr Plot to save
#' @param filename [\code{string(1)}]\cr File name to create on disk

#' @export
Save.Function.ggplot <- function(plot, filename, path, ...){

  complete_file <- base::file.path(path, filename)

  base::print(base::paste0("Trying to save plot: ", complete_file))

  if (base::file.exists(complete_file)){
    input <- utils::menu(title="Do you want to overwrite already existing ggplot?", choices=c("Yes", "No"))
    if(input==1){
      ggplot2::ggsave(plot=plot, filename=filename, path=path, ...)
      base::print("Existing ggplot overwriten")
    }
    else{base::print("ggplot not saved")}

  }
  else{
    ggplot2::ggsave(plot=plot, filename=filename, path=path, ...)
    base::print("New ggplot written")
  }
}
