#' Returns a vector of URLs with special issues from target journal
#' @param journal MDPI journal code
#' @param type "closed", "open" or "all" special issues. "closed" by default.
#' @import magrittr rvest dplyr 
#' @export special_issue_find
#' @return A vector.
#' @examples
#' \dontrun{
#' special_issue_find("covid")
#' }


special_issue_find <- function(journal,type="closed") {
  
  si_url<-data.frame()
  
  message("Compiling all special issues from journal ",journal," with status: ",type)
  
  for (i in 1:100) {
    
    message("Extracting page: ",i)
    
    data<-read_html(paste0("https://www.mdpi.com/journal/",journal,"/special_issues?page_count=100&page_no=",i,"&search=&section_id=&sort=deadline&view=",type))
    
    links<-data%>%
      html_nodes(".article-content")%>%
      html_nodes(".title-link")%>%
      html_attr("href")%>%
      as.data.frame()%>%
      mutate(full_url=paste0("https://www.mdpi.com",.))%>%
      distinct()
    
    si_url<-bind_rows(si_url,links)
    
    if (nrow(si_url)!=nrow(distinct(si_url))){
      si_url<-si_url%>%
        distinct()
      break
    }else{}
    
  }
  
  special_issues<-si_url$full_url
  
  special_issues
  
}


