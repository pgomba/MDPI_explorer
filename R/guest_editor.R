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
    
    editors<-gsub("Dr. |Prof. |Prof. Dr. |Assoc. Prof. ","",editors)%>%
      word(start = 1,end = 2) #Keep two first items of names (see ch v 0.0.1.1)
    
    si_papers<-data%>%
      html_nodes(".article-content")%>%
      html_nodes(".title-link")%>%
      html_attr("href")
    
    deadline<-data%>%
      html_nodes(".si-deadline b")%>%
      html_text2()
    deadline<-gsub("closed |\\(|\\)","",deadline)
    
    
    if (identical(si_papers,character(0))) { 
      temp_df<-data.frame(special_issue=i,num_papers=as.double("empty SI"),prop_flag=as.double("empty SI"))
      special_issues_table<-bind_rows(special_issues_table,temp_df)
      Sys.sleep(sleep)
      count=count+1
      setTxtProgressBar(pb, count)
      
    } else {
      
      table<-data.frame(submitted=as.Date(character()), #ch v 0.0.1.1
                        stringsAsFactors=FALSE)
      
      for (j in si_papers) {
        
        article<-read_html(paste0("https://www.mdpi.com",j))
        
        artictype<-article%>%  # To jump over editorial type papers
          html_nodes(".articletype")%>% # To jump over editorial type papers
          html_text2() # To jump over editorial type papers
        
        if (artictype=="Editorial") { # To jump over editorial type papers
          
        } else {# To jump over editorial type papers
        
        authors<-article%>%
          html_nodes(".art-authors")%>%
          html_nodes(".sciprofiles-link__name")%>%
          html_text2()%>%
          unique()%>%
          word(start = 1,end = 2) #ch v 0.0.1.1
        
        submitted<-article%>% #ch v 0.0.1.1
          html_nodes(".pubhistory span:nth-child(1)")%>%
          html_text2()%>%
          word(start = -3,end = -1)%>% 
          as.Date("%d %B %Y")
        
        flag<-intersect(authors,editors) #outputs 1 when an editor is an author, 0 if not
        
        if (identical(flag,character(0))) { 
          flag<-0
        } else {
          flag<-1
        }
        
        MDPI_url<-j
        temp_df<-data.frame(MDPI_url,flag,submitted)
        
        table<-bind_rows(table,temp_df)
        
        }
        
      }
      
      si_summary<-round(sum(table$flag)/length(table$MDPI_url),3)
      temp_df<-data.frame(special_issue=i,num_papers=length(table$MDPI_url),flags=sum(table$flag),prop_flag=si_summary,deadline=as.Date(deadline,"%d %B %Y"), latest_sub=max(table$submitted))%>%
        mutate(d_over_deadline=deadline-submitted)
      special_issues_table<-bind_rows(special_issues_table,temp_df)
      Sys.sleep(sleep)
      count<-count+1
      setTxtProgressBar(pb, count)
      
    }
    
  }
  special_issues_table
}

test<-guest_editor("covid",1.5)
