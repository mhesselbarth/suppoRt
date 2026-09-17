#' Delete temporary files
#'
#' @description Delete temporary files
#'
#' @param base_dir Character string with the path to the temporary directory.
#' @param minutes_keep Numeric. Age in minutes from which on folders are deleted; more recent folders are kept.
#' @param list_files Logical, whether all files should be listed.
#' @param delete Logical, weather all files should be deleted.
#' @param verbose Logical, whether messages are sent to the console.
#'
#' @details
#' The function deletes all files in the temporary folder which are older than \code{minutes_keep}.
#'
#' @author Code based on scripts by Markus Samek
#'
#' @return (invisible) list
#'
#' @examples
#' \dontrun{
#' clean_temp()
#' }
#'
#' @export
clean_temp <- function(base_dir = dirname(tempdir()), minutes_keep = 0, list_files = FALSE, delete = FALSE, verbose = TRUE) {
    
    # get username
    char_use <- system("whoami", intern = TRUE)

    if (verbose) {

        # current information about temp systems
        char_sys <- system(paste("df -h", base_dir), intern = TRUE)

        df_sys <- data.frame(type = c("Filesystem", "Size", "Used", "Avail", "Use%", "Mounted on"), 
                             value = stringr::str_split(char_sys[2], pattern = "\\s+")[[1]])

        message("> Temporary directory: ", base_dir)

        message("> Used: ", df_sys[df_sys$type == "Used", "value"], " (", df_sys[df_sys$type == "Use%", "value"], ")",
                " | Available: ", df_sys[df_sys$type == "Avail", "value"],
                " | Size: ", df_sys[df_sys$type == "Size", "value"])

        message("> Searching temporary files of [", char_use, "] in <", base_dir, ">")

    }

    # current temp directory
    current_dir <- tempdir()

    # list all present files
    vec_dirs <- list.files(path = base_dir, recursive = FALSE, full.names = TRUE)

    # filter all R related files
    vec_dirs <- vec_dirs[stringr::str_detect(list.files(path = base_dir, recursive = FALSE, all.files = TRUE), pattern = "^Rtmp")]

    # get information about files
    df_meta <- file.info(vec_dirs)
    
    # update some file information
    df_meta$path <- row.names(df_meta)
    df_meta$access <- file.access(names = df_meta$path, mode = 2)
    df_meta$t_diff <- difftime(Sys.time(), df_meta$ctime, units = "mins")

    # only user files
    df_own <- df_meta[df_meta$uname == char_use & !is.na(df_meta$uname), ]

    # no files present
    if (nrow(df_own) == 0) {

        if (verbose) message("> No temporary files of [", char_use, "]")

    # files present
    } else {

        if (verbose) {

            message("> Found [", nrow(df_own), "] temporary files of [", char_use, "]")

            # list all files as vector
            vec_files <- purrr::map(df_own$path, function(i) as.character(list.files(path = i, full.names = TRUE, recursive = TRUE)))
            vec_files <- purrr::simplify(vec_files) 
            
            # calculate size
            num_size <- sum(file.size(vec_files), na.rm = TRUE)
            num_size <- num_size / 1024^2

            message("> Size of temporary file(s): ", format(round(num_size, 2), nsmall = 2), " MB")

        }

        if (list_files) {
         
            message("> Files:\n", paste0("  ~", stringr::str_remove(vec_files, pattern = base_dir), collapse = "\n"))

        } else {

            if (verbose) message("> Use clean_temp(list_files = TRUE) to list all temporary files")
            
        }

        # delete files
        if (delete) {

            # get all files to delete
            df_delete <- df_own[df_own$path != current_dir & df_own$t_diff >= minutes_keep, ]

            if (verbose) message("> Deleting [", nrow(df_delete), "] temporary files")

            try({unlink(x = df_delete$path, recursive = TRUE, force = TRUE)}, silent = TRUE)

        } else {

            if (verbose) message("> Use clean_temp(delete = TRUE) to delete temporary files")

        }
    }

    return(invisible(df_own$path))

}
