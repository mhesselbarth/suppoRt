#' setup_project
#'
#' @description Setup project
#'
#' @param name Name of project to create.
#' @param path Path where project is created.
#' @param folders Vector with names for folders to be created.
#' @param git Logical if git repo should be initialized.
#' @param readme Logical if README.md should be added.
#'
#' @details
#' Create project folder with certain structure.
#'
#' @return void
#'
#' @examples
#' \dontrun{setup_project(name = "Analysis_project", path = "~/Desktop")}
#'
#' @export
setup_project <- function(name, path = NULL, folders = NULL, git = TRUE, readme = TRUE) {

  if (is.null(path)) {path <- here::here()}

  path_complete <- file.path(path, name)

  dir.create(path_complete)

  if (is.null(folders)) {

    folders <- c("Functions", "Data", "Scripts", "Figures", "Various")

    folders <- paste0("0", 1:length(folders), "_", folders)

    sapply(file.path(path_complete, folders), function(x) dir.create(x))

  } else {

    sapply(file.path(path_complete, folders), function(x) dir.create(x))

  }

  message("> Setting up following folders: ", paste(folders, collapse = ", "))

  defaults <- c("Version: 1.0", "", "RestoreWorkspace: Default", "SaveWorkspace: Default",
                "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes",
                "UseSpacesForTab: Yes", "NumSpacesForTab: 2", "Encoding: UTF-8",
                "", "RnwWeave: knitr", "LaTeX: pdfLaTeX",  "AutoAppendNewline: Yes")

  cat(paste0(defaults, collapse = "\n"), file = paste0(path_complete, "/", name, ".Rproj"))

  if (git) {

    gert::git_init(path_complete)

    gitignore <- c(".Rproj.user", ".Rhistory", ".RData", ".Ruserdata", ".DS_Store")

    cat(paste0(gitignore, collapse = "\n"), file = paste0(path_complete, "/.gitignore"))

    message("> Setting up git repository.")

  }

  if (readme) {

    cat("\n# README\n\n<!-- badges: start -->\n<!-- badges: end -->\n\nThe goal of", name, "is to ...",
        file = paste0(path_complete, "/README.md"))

    message("> Adding README file.")


  }

  message("\n> All done! Please go to '", path_complete, "' to find project.")

  if (git) {

    message("> Please remember to initiate first git commit.")

  }
}
