#' @title suppoRt
#'
#' @description
#' Collection of helper functions

#' @name suppoRt
#' @docType package
#' @useDynLib suppoRt
#' @importFrom Rcpp sourceCpp
# nocov start
"_PACKAGE"

globalVariables(c(
  "hi",
  "is",
  "lo",
  "obs",
  "r",
  "theo",
  "type"))

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# nocov end
