#' Function to save rds objects
#'
#' This function checks if the file aready exists before saving it.
#' See ?base::saveRDS() for more information
#' @param object [\code{R object(1)}]\cr R object to serialize
#' @param file [\code{string(1)}]\cr Connection or the name of the file where the R object is saved

#' @export
Save.Function.rds <- function(object,  filename = NULL, path = NULL, overwrite = FALSE, ...){

  if(is.null(path)){path <- getwd()}
  if(is.null(filename)){filename <- 'rds_file'}

  complete_file <- file.path(path, filename)
  cat("Trying to save file: ", complete_file, "\n\n")

  if (base::file.exists(file)){
    if(overwrite == TRUE){
      saveRDS(object=object, file=complete_file, ...)
      print("Existing file overwriten \n")
    }
    else{print("File not saved \n")}
  }

  else{
    saveRDS(object=object, file=file, ...)
    cat("New file written \n")
  }
}
