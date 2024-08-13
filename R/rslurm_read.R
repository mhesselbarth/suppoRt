#' rslurm_read
#'
#' @description rslurm read
#'
#' @param x slurm_job object.
#' @param pattern String with pattern to match.
#' @param as_list Logical if result should be returned as \code{list} or \code{data.frame}.
#'
#' @details
#' This functions reads all \code{.rds} files matching the \code{pattern} argument.
#' This can be useful if results were not saved using the default templates of the
#' \code{rslurm} package. For more information, please see the \code{rslurm} package.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' rslurm_read(x = sbatch_results)
#' }
#'
#' @export
rslurm_read <- function(x, pattern, as_list = TRUE) {

  if (!inherits(x = x, what = "slurm_job")) {
    stop("Please provide 'slurm_job' object.", call. = FALSE)
  }

  tmpdir <- paste0("_rslurm_", x$jobname)
  message("Looking for .RDS-files in ", tmpdir)

  res_files <- list.files(path = tmpdir, pattern = pattern)

  if (length(res_files) != x$nodes) {
    warning("Some .RDS-files are missing.", call. = FALSE)
  }

  r <- lapply(X = file.path(tmpdir, res_files), function(i) readRDS(i))

  if (length(r) == 0) {stop("No .RDS files found", call. = FALSE)}

  if (!as_list) {
    r <- do.call(what = "rbind", args = r)
  }

  return(r)

}
