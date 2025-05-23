% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup-project.R
\name{setup_project}
\alias{setup_project}
\title{setup_project}
\usage{
setup_project(name, path = NULL, folders = NULL, git = TRUE, readme = TRUE)
}
\arguments{
\item{name}{Name of project to create.}

\item{path}{Path where project is created.}

\item{folders}{Vector with names for folders to be created.}

\item{git}{Logical if git repo should be initialized.}

\item{readme}{Logical if README.md should be added.}
}
\value{
void
}
\description{
Setup project
}
\details{
Create project folder with certain structure.
}
\examples{
\dontrun{setup_project(name = "Analysis_project", path = "~/Desktop")}

}
