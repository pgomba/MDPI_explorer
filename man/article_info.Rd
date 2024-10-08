% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/article_info.R
\name{article_info}
\alias{article_info}
\title{This function extracts key editorial information from one or more paper URLs. Specifically, it retrieves the submission, revision, and acceptance dates, as well as the article type. The function also calculates the turnaround time (the duration from submission to acceptance) and identifies whether the paper is part of a special issue.}
\usage{
article_info(vector, sleep = 2, sample_size, show_progress = TRUE)
}
\arguments{
\item{vector}{A vector with urls.}

\item{sleep}{Number of seconds between scraping iterations. 2 sec. by default}

\item{sample_size}{A number. How many papers do you want to explore from the main vector. Leave blank for all}

\item{show_progress}{Logical. If \code{TRUE}, a progress bar is displayed during the function execution. Defaults to \code{TRUE}.}
}
\value{
A data frame (class: \code{data.frame}) with the following columns:
\describe{
\item{i}{The URL of the article from which the information is retrieved.}
\item{article_type}{The classification of the article (e.g., editorial, review).}
\item{Received}{The date the article was received by the publisher.}
\item{Accepted}{The date the article was accepted for publication.}
\item{tat}{The turnaround time, calculated as the number of days between the received and accepted dates.}
\item{year}{The year in which the article was accepted for publication.}
\item{issue_type}{Indicates whether the article is part of a special issue.}
}
}
\description{
This function extracts key editorial information from one or more paper URLs. Specifically, it retrieves the submission, revision, and acceptance dates, as well as the article type. The function also calculates the turnaround time (the duration from submission to acceptance) and identifies whether the paper is part of a special issue.
}
\examples{
url<-c("https://www.mdpi.com/2073-4336/8/4/45","https://www.mdpi.com/2073-4336/11/3/39")
info<-article_info(url, 1.5)

}
