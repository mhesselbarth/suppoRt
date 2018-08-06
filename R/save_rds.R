#' Function to save rds objects
#'
#' This function checks if the file aready exists before saving it.
#' See ?base::saveRDS() for more information
#' @param object [\code{R object(1)}]\cr R object to serialize
#' @param filename [\code{string(1)}]\cr Name of the file where the R object is saved. The file extension must be .rds
#' @param path [\code{string(1)}]\cr Path to where the R object is saved
#' @param overwrite [\code{logical(1)}]\cr Overwrite if the file alreay exists

#' @export
save_rds <- function(object,  filename = NULL, path = NULL, overwrite = FALSE, ...){

  if(is.null(path)){path <- getwd()}
  if(is.null(filename)){filename <- 'rds_file.rds'}

  complete_file <- file.path(path, filename)
  cat("Trying to save file: ", complete_file, "\n\n")

  if (base::file.exists(complete_file)){
    if(overwrite == TRUE){
      saveRDS(object=object, file=complete_file, ...)
      cat("Existing file overwriten \n")
    }
    else{cat("File not saved \n")}
  }

  else{
    saveRDS(object=object, file=complete_file, ...)
    cat("New file written \n")
  }
}
