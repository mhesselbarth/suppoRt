% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update-pkgs.R
\name{update_pkgs}
\alias{update_pkgs}
\title{update_pkgs}
\usage{
update_pkgs(exclude = NULL)
}
\arguments{
\item{exclude}{Vector with package names not to update.}
}
\value{
void
}
\description{
Update packages
}
\details{
The function updates all installed packages using the \code{remotes} package.
It allows to exclude package (e.g., because only available in non-public GitHub repo).
}
\examples{
\dontrun{
update_pkgs(exclude = c("pkg_a", "pkg_b"))
}

}
