% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/special_issue_find.R
\name{special_issue_find}
\alias{special_issue_find}
\title{Retrieves all special issues of a specified journal with URLs. Filters results by issue status (open, closed, or all) and optional year range.}
\usage{
special_issue_find(journal, type = "closed", years = NULL, verbose = TRUE)
}
\arguments{
\item{journal}{MDPI journal code}

\item{type}{"closed", "open" or "all" special issues. "closed" by default.}

\item{years}{A vector containing special issues closure dates to limit the search to certain years}

\item{verbose}{Logical. If \code{TRUE}, informative messages will be printed during the function execution. Defaults to \code{TRUE}.}
}
\value{
A vector.
}
\description{
Retrieves all special issues of a specified journal with URLs. Filters results by issue status (open, closed, or all) and optional year range.
}
\examples{
\dontrun{
special_issue_find("covid")
}
}
