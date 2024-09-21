#' Calculates number of authors selfcitations against all references
#' 
#' @param article_url A valid MDPI article url
#' @import magrittr rvest dplyr stringr
#' @export selfcite_check
#' @return A string.
#' @examples
#' \dontrun{
#' selfcite_check(paper_url)
#' }


selfcite_check <- function(article_url) {
  
  data<-read_html(article_url)
  
  paper_authors<-data%>% #Pick APA citation style from menu
    html_nodes("p:nth-child(2)")%>%
    html_text2()%>%
    .[[6]]%>%
    sub("(.*?\\.)\\s+[^\\.]*\\..*", "\\1", .)%>%
    str_split(pattern = ";")%>%
    unlist()%>%
    trimws()
  
  string_to_remove <- "et al."
  
  paper_authors<-setdiff(paper_authors, string_to_remove)
  
  article_references<-data%>%
    html_nodes("#html-references_list li")%>%
    html_text2()%>%
    sub("(.*?\\.)\\s+[^\\.]*\\..*", "\\1", .)
  
  table<-data.frame()
  
  for (i in 1:length(article_references)) {
    
    references_names_vector<-article_references[[i]]%>%
      str_split(pattern = ";")%>%
      unlist()%>%
      trimws()
    
    string_to_remove <- "et al."
    
    references_names_vector<-setdiff(references_names_vector, string_to_remove)
    
    matches <- sapply(references_names_vector, function(authors) {
      any(authors %in% paper_authors)
    })
    
    positive<-sum(matches)
    
    temp_df<-data.frame(ref_n=i,positive)
    
    table<-bind_rows(table,temp_df)
    
  }
  
  self_cite<-paste(nrow(table%>%filter(positive>0)),nrow(table))
  print(self_cite)
  
  
}

