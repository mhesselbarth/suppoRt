#' Delete temporary files
#'
#' @description
#' Function to delete temporary files
#'
#' @param base_dir Character with path to temp files.
#' @param minutes_keep Numeric with threshold in minutes for files to keep.
#' @param list_files Logical if all files should be listed.
#' @param delete Logical if all files should be deleted.
#' @param verbose Logical if messages are send to console.
#'
#' @details
#' The function deletes all files in the temporary folder which are older than \code{minutes_keep}.
#'
#' @author Code based on scripts of Markus Samek
#'
#' @return (invisible) list
#'
#' @examples
#' \dontrun{
#' clean_temp()
#' }
#'
#' @export
clean_temp <- function(base_dir = dirname(tempdir()), minutes_keep = 0, list_files = FALSE, delete = FALSE, verbose = getOption("energyTools.verbose", TRUE)) {
    
    # Username auslesen
    char_use <- system("whoami", intern = TRUE)

    # An Konsole senden
    if (verbose) {

        # aktuelle Auslastung von Konsole auslesen
        char_sys <- system(paste("df -h", base_dir), intern = TRUE)

        df_sys <- data.frame(type = c("Filesystem", "Size", "Used", "Avail", "Use%", "Mounted on"), 
                             value = stringr::str_split(char_sys[2], pattern = "\\s+")[[1]])

        message("> Tempor\u{00E4}res Verzeichnis: ", base_dir)

        message("> Belegt: ", df_sys[df_sys$type == "Used", "value"], " (", df_sys[df_sys$type == "Use%", "value"], ")",
                " | Verf\u{00FC}gbar: ", df_sys[df_sys$type == "Avail", "value"],
                " | Gesamtgr\u{00F6}\u{00DF}e: ", df_sys[df_sys$type == "Size", "value"])

        message("> Tempor\u{00E4}re Ordner von [", char_use, "] in <", base_dir, "> suchen")

    }

    # Ordner der aktuellen Session
    current_dir <- tempdir()

    # alle vorhandenen Temp Ordner listen
    vec_dirs <- list.files(path = base_dir, recursive = FALSE, full.names = TRUE)

    # alle R Temp Ordner filtern
    vec_dirs <- vec_dirs[stringr::str_detect(list.files(path = base_dir, recursive = FALSE), pattern = "^Rtmp")]

    # alle Metadaten auslesen
    df_meta <- file.info(vec_dirs)
    
    # Metadaten updaten
    df_meta$path <- row.names(df_meta)
    df_meta$access <- file.access(names = df_meta$path, mode = 2)
    df_meta$t_diff <- difftime(Sys.time(), df_meta$ctime, units = "mins")

    # eigene Ordner filtern
    df_own <- df_meta[df_meta$uname == char_use & !is.na(df_meta$uname), ]

    # keine Ordner vorhanden
    if (nrow(df_own) == 0) {

        if (verbose) message("> Keine tempor\u{00E4}ren Ordner von [", char_use, "] gefunden")

    # Ordner vorhanden
    } else {

        if (verbose) {

            message("> Insgesamt [", nrow(df_own), "] tempor\u{00E4}re Ordner von [", char_use, "] gefunden")

            # Alle Datein listen
            vec_files <- purrr::map(df_own$path, function(i) as.character(list.files(i, full.names = TRUE, recursive = TRUE)))
            vec_files <- purrr::simplify(vec_files) 
            
            # Größe berechnen (file.size liefert Bytes)
            num_size <- sum(file.size(vec_files), na.rm = TRUE)
            num_size <- num_size / 1024^2

            message("> Gr\u{00F6}\u{00DF}e tempor\u{00E4}re File(s): ", format(round(num_size, 2), nsmall = 2), " MB")

        }

        if (list_files) {
         
            message("> Files:\n", paste0("  ~", stringr::str_remove(vec_files, pattern = base_dir), collapse = "\n"))

        } else {

            if (verbose) message("> clean_temp(list_files = TRUE) ausf\u{00FC}hren um Datein anzuzeigen")
            
        }

        # Ordner löschen
        if (delete) {

            # Alle Ordner zum löschen
            df_delete <- df_own[df_own$path != current_dir & df_own$t_diff >= minutes_keep, ]

            if (verbose) message("> L\u{00F6}schen von [", nrow(df_delete), "] tempor\u{00E4}re Ordnern")

            try({unlink(x = df_delete$path, recursive = TRUE, force = TRUE)}, silent = TRUE)

        } else {

            if (verbose) message("> clean_temp(delete = TRUE) ausf\u{00FC}hren um Ordner zu l\u{00F6}schen")

        }
    }

    return(invisible(df_own$path))

}
