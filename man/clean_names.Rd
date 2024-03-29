% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_names.R
\name{clean_names}
\alias{clean_names}
\title{Homogenize editors and authors names}
\usage{
clean_names(name_vector)
}
\arguments{
\item{name_vector}{A string with names separated by commas}
}
\value{
A vector
}
\description{
Takes a vector of names to return the names without abbreviated middle names, academic titles and hyphens.
}
\examples{
clean_names(c("Matthias M. Bauer","Thomas Garca Morrison","Wolfgang Nitsche", "Elias Biobaca L." ))
}
