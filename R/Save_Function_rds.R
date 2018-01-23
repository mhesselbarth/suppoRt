#' Function to save rds objects
#'
#' This function checks if the file aready exists. See ?base::saveRDS() for more information

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
