#' Retrieves all special issues of a specified journal with URLs. Filters results by issue status (open, closed, or all) and optional year range.
#' @param journal MDPI journal code
#' @param type "closed", "open" or "all" special issues. "closed" by default.
#' @param years A vector containing special issues closure dates to limit the search to certain years
#' @param verbose Logical. If `TRUE`, informative messages will be printed during the function execution. Defaults to `TRUE`.
#' @import magrittr rvest dplyr 
#' @export special_issue_find
#' @return A vector.
#' @examples
#' \donttest{
#' special_issue_find("covid")
#' }


special_issue_find <- function(journal,type="closed",years=NULL,verbose=TRUE) {
  
  si_url<-data.frame()
  
  if (verbose) {
  
  message("Compiling all special issues from journal ",journal," with status: ",type)
  
  }
    
  for (i in 1:100) {
    
    if (verbose) {
    
    message("Extracting page: ",i)
      
    }
    
    data<-read_html(paste0("https://www.mdpi.com/journal/",journal,"/special_issues?page_count=100&page_no=",i,"&search=&section_id=&sort=deadline&view=",type))
    
    links<-data%>%
      html_nodes(".article-content")%>%
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
    
    si_url<-bind_rows(si_url,links)
    
    if (nrow(si_url)!=nrow(distinct(si_url))){
      si_url<-si_url%>%
        distinct()
      break
    }else{}
    
  }
  
  if (is.null(years)) {
    
    special_issues<-si_url$full_url
    
  }else{
    special_issues<-si_url%>%
      filter(year %in% years)
    
    special_issues<-special_issues$full_url
  }
  
  special_issues
  
}


