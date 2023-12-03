#' Extracts all urls on journal sitemap but keeps only papers. Outputs a vector with urls
#' @import magrittr rvest
#' @param journal A string containing the name of a MDPI journal
#' @return A vector
#' @export journal_papers
#' @examples
#' journal_papers("agronomy")

journal_papers <- function(journal) {

  sitemap<-read_html(paste0("https://www.mdpi.com/sitemap/sitemap.",journal,".xml"))
  
  links<-sitemap%>% #Here we obtain all links from the sitemap, but needs some cleaning
    html_nodes("loc")%>%
    html_text2()%>%
    as.data.frame()%>%
    mutate(slash_number=str_count(., "/"))%>% #count number of slashes in url for further cleaning
    filter(!grepl(journal,.))%>% #removing all links that include the name of the journal - these are not papers
    filter(!grepl("issue", .)) %>% #remove links for special issues - still not papers    
    filter(slash_number>4) # paper url have more than 4 slashes
  
  papers_vector<-links$. #Creating a vector to be later used in the loop
  
  papers_vector
}


