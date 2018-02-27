#' Function to save rds objects
#'
#' This function checks if the file aready exists before saving it.
#' See ?base::saveRDS() for more information
#' @param object [\code{R object(1)}]\cr R object to serialize
#' @param file [\code{string(1)}]\cr Connection or the name of the file where the R object is saved

#' @export
Save.Function.rds <- function(object, file, ...){
  if (base::file.exists(file)){
    input <- utils::menu(title="Do you want to overwrite already existing file?", choices=c("Yes", "No"))
    if(input==1){
      base::saveRDS(object=object, file=file, ...)
      base::print(base::paste0("Existing file ", file, " overwriten"))
    }
    else{base::print("File not saved")}
  }

  else{
    base::saveRDS(object=object, file=file, ...)
    base::print(base::paste0("New file ", file, " written"))
  }
}
