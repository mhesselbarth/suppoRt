#' @title suppoRt
#'
#' @description
#' Collection of helper functions

#' @name suppoRt
#' @docType package
#' @keywords internal
"_PACKAGE"

globalVariables(c(
  "change_time",
  "hi",
  "is",
  "lo",
  "obs",
  "path",
  "r",
  "theo",
  "type", 
  "t_diff", 
  "user"))

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE