#' This function retrieves the URLs for all published articles from a specified journal. Users can provide the journal's code 'see MDPI_journals.rda', and the function will return the URLs of all articles available within the journal.
#' @import magrittr rvest
#' @param journal A string containing the name of a MDPI journal
#' @return A vector (class: \code{character}) containing a list of articles URLs from target journal
#' @export article_find
#' @examples
#' \donttest{
#' agr_articles<-article_find("agriculture")
#' }

article_find <- function(journal) {

  sitemap<-read_html(paste0("https://www.mdpi.com/sitemap/sitemap.",journal,".xml"))%>% #Here we obtain all links from the sitemap, but needs some cleaning
    html_nodes("loc")%>%
    html_text2()
  sitemap2<- tryCatch(expr = {read_html(paste0("https://www.mdpi.com/sitemap/sitemap.",journal,"_0.xml"))%>% #Here we obtain all links from the sitemap, but needs some cleaning
      html_nodes("loc")%>%
      html_text2()},
      error=function(e){
        valid="Journal has only one sitemap"
      })
  
  if(exists("valid")==TRUE){
    sitemap<-sitemap
  }else{
    sitemap<-c(sitemap,sitemap2)
  }
  
  links<-sitemap%>% #Here we obtain all links from the sitemap, but needs some cleaning
    as.data.frame()%>%
    distinct()%>%
    mutate(slash_number=str_count(., "/"))%>% #count number of slashes in url for further cleaning
    filter(!grepl(journal,.))%>% #removing all links that include the name of the journal - these are not papers
    filter(!grepl("issue", .)) %>% #remove links for special issues - still not papers    
    filter(slash_number>4) # paper url have more than 4 slashes
  
  papers_vector<-links$. #Creating a vector to be later used in the loop
  
  papers_vector
}


