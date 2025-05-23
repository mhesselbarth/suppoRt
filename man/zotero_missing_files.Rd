% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zotero-missing-files.R
\name{zotero_missing_files}
\alias{zotero_missing_files}
\title{zotero_missing_files}
\usage{
zotero_missing_files(
  lib_file = file.choose(),
  file_folder = rstudioapi::selectDirectory()
)
}
\arguments{
\item{lib_file}{A string with the path to the Zotero library exported as a CSV file.}

\item{file_folder}{A string with the path to the folder containing the files
linked to the references in the Zotero library.}
}
\value{
vector
}
\description{
Find missing PDF files
}
\details{
This function compares the files in a folder with the files linked to the
references in a Zotero library and returns the names of the missing PDF files.
}
\examples{
\dontrun{zotero_missing_files()}

}
\references{
Adapted from Daniel Vartanian (https://gist.github.com/danielvartan/924817b7e4b69212beb217f339c37a3f)
}
