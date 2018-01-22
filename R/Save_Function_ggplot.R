#' Function to save ggplot
#'
#' This function checks if the file aready exists. See ?ggplot2::ggsave() for more information

#' @export
Save.Function.ggplot <- function(file=stop("'file' must be specified"), ...){
  if (file.exists(file)){
    input <- menu(title="Do you want to overwrite already existing file?", choices=c("Yes", "No"))
    if(input==1){
      ggplot2::ggsave(filename=file, ...)
      print("Existing file overwriten")
    }
    else{print("File not saved")}

  }
  else{
    ggplot2::ggsave(filename=file, ...)
    print("New file written")
  }
}
