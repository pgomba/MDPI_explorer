#' Extracts data from special issues, including guest editors' paper counts (excluding editorials), time between last submission and issue closure, and whether guest editors served as academic editors for any published papers.
#' 
#' @param journal_urls A list of MDPI special issues urls
#' @param sample_size A number. How many special issues do you want to explore from the main vector. Leave blank for all
#' @param sleep Number of seconds between scraping iterations. 2 sec. by default
#' @import magrittr rvest dplyr 
#' @export guest_editor_info
#' @return A data frame.
#' @examples
#' \dontrun{
#' guest_editor_info("covid", 1.5)
#' }


guest_editor_info <- function(journal_urls, sample_size, sleep=2) {
  
  if (missing(sample_size)) {
    sample_size=length(journal_urls)
  }else{
    sample_size=sample_size
  }
  
  urls<-sample(journal_urls,sample_size)
  
  special_issues_table<-data.frame(special_issue=character(),
                                   num_papers=double(),
                                   flags=numeric(),
                                   prop_flag=double(),
                                   stringsAsFactors=FALSE)
  
  
  pb <- txtProgressBar(min = 0, max = length(urls), initial = 0,style=3) #Build progress bar
  count<-0
  
  for (i in urls) {
    
    data<-read_html(paste0(i,"#editors"))
    
    editors_part1<-data%>% 
      html_nodes(".smaller-pictures .sciprofiles-link__name")%>%
      html_text2()
    
    editors_part2<-data%>%
        html_nodes(".smaller-pictures strong")%>%
        html_text2()
    
    editors<-append(editors_part1,editors_part2)%>%
      clean_names()%>%
      unique()
    
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
      
      table<-data.frame(submitted=as.Date(character()), 
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
          unique()
        
        authors<-clean_names(authors)
        
        academic_editor<-article%>%
          html_nodes(".academic-editor-container")%>%
          html_nodes(".sciprofiles-link__name")%>%
          html_text2()%>%
          unique()%>%
          clean_names()
        
        if (identical(academic_editor,character(0))) {
          aca_flag<-"No info"
        } else {
          
          aca_flag<-intersect(editors,academic_editor)
          
          if (identical(aca_flag,character(0))) {
            aca_flag<-0
          } else {
            aca_flag<-1}}
        
      
        guest_editor<-length(editors)
        result <- as.numeric(editors %in% authors)%>%
          paste(., collapse = ", ")
        
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
        temp_df<-data.frame(MDPI_url,flag,result,submitted,aca_flag) ##beta
        temp_df$aca_flag<-as.character(aca_flag)
        
        table<-bind_rows(table,temp_df)
        
        }
        
      }
      
      si_summary<-round(sum(table$flag)/length(table$MDPI_url),3)
      
      rt_sum_vector<-rep(0,guest_editor)
      
      for (j in 1:nrow(table)) {
        
        line<-table$result[j]%>%
          str_split(",")%>%
          unlist()%>%
          as.numeric()
        
        rt_sum_vector<-rt_sum_vector+line
      }
      
      rt_sum_vector2<-rt_sum_vector%>%
        paste(collapse = ",")
      
      
      if (any(table$aca_flag == "No info")) {
        # Do something if "No info" is found
        aca_flag<-"No info"
      } else {
        aca_flag<-as.character(sum(as.numeric(table$aca_flag)))
      }
      
      
      temp_df<-data.frame(special_issue=i,num_papers=length(table$MDPI_url),flags=sum(table$flag),prop_flag=si_summary,deadline=as.Date(deadline,"%d %B %Y"), latest_sub=max(table$submitted),rt_sum_vector2,aca_flag)%>%
        mutate(d_over_deadline=deadline-latest_sub)
      special_issues_table<-bind_rows(special_issues_table,temp_df)
      Sys.sleep(sleep)
      count<-count+1
      setTxtProgressBar(pb, count)
      
    }
    
  }
  special_issues_table
}

