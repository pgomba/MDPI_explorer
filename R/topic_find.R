#' Retrieves all topics of a specified journal with URLs. Filters results by issue status (open, closed, or all) and optional year range.
#' @param journal MDPI journal code
#' @param type "closed", "open" or "all" topics. "closed" by default.
#' @param years A vector containing topics closure dates to limit the search to certain years
#' @param verbose Logical. If `TRUE`, informative messages will be printed during the function execution. Defaults to `TRUE`.
#' @import magrittr rvest dplyr 
#' @export topic_find
#' @return A vector.
#' @examples
#' \dontrun{
#' topic_find("covid")
#' }


topic_find <- function(journal,type="closed",years=NULL,verbose=TRUE) {
  
  topic_url<-data.frame()
  
  if (verbose) {
  
  message("Compiling all Topics from journal ",journal," with status: ",type)
  
  }
    
  for (i in 1:100) {
    
    if (verbose) {
    
    message("Extracting page: ",i)
      
    }
  
    data<-read_html(paste0("https://www.mdpi.com/topics?query=&journal=",journal,"&status=",type,"&category=all&page_no=",i))
    
    links<-data%>%
      html_nodes(".title-link")%>%
      html_attr("href")%>%
      as.data.frame()%>%
      mutate(full_url=paste0("https://www.mdpi.com",.))%>%
      distinct()
    
    year<-data%>%
      html_nodes("strong:nth-child(3)")%>%
      html_text2()%>%
      gsub(".* ","",.)%>%
      as.data.frame()%>%
      rename(year=1)
    
    links<-cbind(links,year)
    
    topic_url<-bind_rows(topic_url,links)
    
    if (nrow(links)==0){
      topic_url<-topic_url%>%
        distinct()
      break
    }else{}
    
  }
  
  if (is.null(years)) {
    
    topic_issues<-topic_url$full_url
    
  }else{
    topic_issues<-topic_url%>%
      filter(year %in% years)
    
    topic_issues<-topic_issues$full_url
  }
  
  topic_issues
  
}


