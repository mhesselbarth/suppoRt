#' Function to save rds objects
#'
#' This function checks if the file aready exists. See ?base::saveRDS() for more information
#' @param object [\code{R object(1)}]\cr R object to save
#' @param file [\code{string(1)}]\cr Path and name where the file is saved

#' @export
Save.Function.rds <- function(file=stop("'file' must be specified"), ...){
  if (file.exists(file)){
    input <- menu(title="Do you want to overwrite already existing file?", choices=c("Yes", "No"))
    if(input==1){
      base::saveRDS(file=file, ...)
      print(paste0("Existing file ", file, " overwriten"))
    }
    else{print("File not saved")}
  }

  else{
    base::saveRDS(file=file, ...)
    print(paste0("New file ", file, " written"))
  }
}
