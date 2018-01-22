#' Set path to working directory
#'
#' This function automatically detects the operating system and sets the path
#' @return [\code{String(1)}]\cr WD for Hainich scripts

#' @export
Set.Path <- function(){
  os <- Sys.info()[[1]]
  print(paste0("Selected operating system: ", os))

  if(os=="Windows"){
    p <- "C:/Users/Maximilian/ownCloud/08_Hainich/"
  }

  else if(os=="Darwin"){
    p <- "~/ownCloud/08_Hainich/"
  }

  else if(os=="Linux"){
    p <- "/home/max/Cloud_PhD/"
  }

  else{
    p <- NULL
    print("Select either Windows, Mac or Server")
  }
  print(paste0("Selected path: ", p))
  return(p)
}
