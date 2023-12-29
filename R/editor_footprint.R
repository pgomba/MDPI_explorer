#' Finds data on papers published by guest editors in their own special issues
#' @param journal MDPI journal code
#' @param sleep Number of seconds between scraping iterations
#' @import magrittr rvest dplyr 
#' @export guest_editor
#' @return A data frame.
#' @examples
#' \dontrun{
#' guest_editor("covid", 1.5)
#' }


guest_editor <- function(journal, sleep) {
  
  si_url<-data.frame()
  for (i in 1:100) {
    
    data<-read_html(paste0("https://www.mdpi.com/journal/",journal,"/special_issues?page_count=100&page_no=",i,"&search=&section_id=&sort=deadline&view=closed"))
    
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
  
  special_issues_table<-data.frame(special_issue=character(),
                                   num_papers=double(),
                                   flags=numeric(),
                                   prop_flag=double(),
                                   stringsAsFactors=FALSE)
  
  
  pb <- txtProgressBar(min = 0, max = length(special_issues), initial = 0,style=3) #Build progress bar
  count<-0
  
  for (i in special_issues) {
    
    data<-read_html(paste0(i,"#editors"))
    
    editors<-data%>%
      html_nodes(".smaller-pictures .sciprofiles-link__name")%>%
      html_text2()
    
    editors<-gsub("Dr. |Prof. |Prof. Dr. ","",editors)
    
    si_papers<-data%>%
      html_nodes(".article-content")%>%
      html_nodes(".title-link")%>%
      html_attr("href")
    
    deadline<-data%>%
      html_nodes(".si-deadline b")%>%
      html_text2()
    deadline<-gsub("closed |\\(|\\)","",deadline)
    
    last_published<-data%>%
      html_nodes(".color-grey-dark")%>%
      html_text2()
    last_published<-word(last_published,start = -3,end = -1)
    last_published<-as.Date(last_published,"%d %B %Y")
    last_published<-max(last_published)
    
    
    if (identical(si_papers,character(0))) { 
      temp_df<-data.frame(special_issue=i,num_papers=as.double("empty SI"),prop_flag=as.double("empty SI"))
      special_issues_table<-bind_rows(special_issues_table,temp_df)
      Sys.sleep(sleep)
      count=count+1
      setTxtProgressBar(pb, count)
      
    } else {
      
      table<-data.frame(special_issue=character(),
                        num_papers=double(),
                        flags=numeric(),
                        prop_flag=double(),
                        stringsAsFactors=FALSE)
      
      for (j in si_papers) {
        
        article<-read_html(paste0("https://www.mdpi.com",j))
        
        authors<-article%>%
          html_nodes(".art-authors")%>%
          html_nodes(".sciprofiles-link__name")%>%
          html_text2()%>%
          unique()
        
        flag<-intersect(authors,editors) #outputs 1 when an editor is an author, 0 if not
        
        if (identical(flag,character(0))) { 
          flag<-0
        } else {
          flag<-1
        }
        
        MDPI_url<-j
        temp_df<-data.frame(MDPI_url,flag)
        
        table<-bind_rows(table,temp_df)
        
      }
      
      si_summary<-round(sum(table$flag)/length(table$MDPI_url),2)
      temp_df<-data.frame(special_issue=i,num_papers=length(table$MDPI_url),flags=sum(table$flag),prop_flag=si_summary,deadline=as.Date(deadline,"%d %B %Y"), last_published)%>%
        mutate(d_over_deadline=last_published-deadline)
      special_issues_table<-bind_rows(special_issues_table,temp_df)
      Sys.sleep(sleep)
      count<-count+1
      setTxtProgressBar(pb, count)
      
    }
    
  }
  special_issues_table
}
