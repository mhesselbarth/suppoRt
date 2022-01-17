#' rslurm_missing
#'
#' @description rslurm missing
#'
#' @param x slurm_job object.
#'
#' @details
#' The function checks if all .RDS files are present in the \code{rslurm_job} project.
#' For more information, please see the \code{rslurm} package.
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' rslurm_missing(x = sbatch_results)
#' }
#'
#' @aliases rslurm_missing
#' @rdname rslurm_missing

#' @export
rslurm_missing <- function(x) {

  if (!inherits(x = x, what = "slurm_job")) {

    stop("Please provide 'slurm_job' object.", call. = FALSE)

  }

  res_files <- paste0("results_", 0:(x$nodes - 1), ".RDS")

  tmpdir <- paste0("_rslurm_", x$jobname)

  message("Looking for .RDS-files in ", tmpdir)

  missing_files <- setdiff(res_files, dir(path = tmpdir))

  if (length(missing_files) == 0) {

    message("Hooray! No missing .RDS-files.")

    return(NA)

  } else {

    warning("Oh nooo! ", length(missing_files), " .RDS-files are missing.")

    return(missing_files)

  }
}
