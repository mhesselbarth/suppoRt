#' Function to save ggplot
#'
#' This function checks if the file aready exists. See ?ggplot2::ggsave() for more information
#' @param object [\code{R object(1)}]\cr R object to save
#' @param file [\code{string(1)}]\cr Path and name where the file is saved

#' @export
Save.Function.ggplot <- function(object, file, ...){
  if (file.exists(file)){
    input <- menu(title="Do you want to overwrite already existing file?", choices=c("Yes", "No"))
    if(input==1){
      ggplot2::ggsave(plot=object, filename=file, ...)
      print("Existing file overwriten")
    }
    else{print("File not saved")}

  }
  else{
    ggplot2::ggsave(plot=object, filename=file, ...)
    print("New file written")
  }
}
