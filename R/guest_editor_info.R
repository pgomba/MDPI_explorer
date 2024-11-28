#' Obtain information from guest edited special issues
#'
#' @description
#' **Deprecated**: This function is deprecated and will be removed in a future version of the package. 
#' Use `special_issue_info()` instead. It extracts data from special issues, including guest editors' paper counts 
#' (excluding editorials), time between last submission and issue closure, and whether guest editors served 
#' as academic editors for any published papers.
#' 
#' @param journal_urls A list of MDPI special issues URLs
#' @param sample_size A number. How many special issues do you want to explore from the main vector. Leave blank for all
#' @param show_progress Logical. If `TRUE`, a progress bar is displayed during the function execution. Defaults to `TRUE`.
#' @param sleep Number of seconds between scraping iterations. 2 sec. by default
#' @import magrittr rvest dplyr 
#' @export guest_editor_info
#' @return A data frame (class: \code{data.frame}) with the following columns:
#' \describe{
#'   \item{special_issue}{The URL of the special issue from which the information is retrieved.}
#'   \item{num_papers}{Number of special issues contained in the special issue, not considering editorial type articles}
#'   \item{flags}{Number of articles in the special issue with guest editorial pressence}
#'   \item{prop_flag}{Proportion of articles in the special issue in which a guest editor is present}
#'   \item{deadline}{Time at which the special issue was or will be closed}
#'   \item{latest_sub}{Time at which last article present in the special issue was submitted}
#'   \item{rt_sum_vector2}{Numeric vector showing number of articles in which each individual guest editor is present}
#'   \item{aca_flag}{Number of articles in the special issue where the academic editor is a guest editor too}
#'   \item{d_over_deadline}{Day differential between special issue closure and latest article submission}
#' }
#' @examples
#' \donttest{
#' ge_issue<-"https://www.mdpi.com/journal/plants/special_issues/5F5L5569XN"
#' ge_info<-guest_editor_info(ge_issue)
#' }
#' 


guest_editor_info <- function(journal_urls, sample_size, sleep=2,show_progress=TRUE) {
  
  warning(
    "The `old_function()` is deprecated and will be removed in a future version. ",
    "Please use `special_issue_info()` instead.",
    call. = FALSE
  )
  
  special_issue_info(journal_urls, sample_size, sleep=2,show_progress=TRUE)
  
 
}

