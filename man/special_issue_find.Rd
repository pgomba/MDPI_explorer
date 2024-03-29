% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/special_issue_find.R
\name{special_issue_find}
\alias{special_issue_find}
\title{Returns a vector of URLs with special issues from target journal}
\usage{
special_issue_find(journal, type = "closed")
}
\arguments{
\item{journal}{MDPI journal code}

\item{type}{"closed", "open" or "all" special issues. "closed" by default.}
}
\value{
A vector.
}
\description{
Returns a vector of URLs with special issues from target journal
}
\examples{
\dontrun{
special_issue_find("covid")
}
}
