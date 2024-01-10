#' Extracts data for each individual paper
#' @param vector A vector with urls.
#' @param sleep Number of seconds between scraping iterations. 2 sec. by default
#' @param sample_size A number. How many papers do you want to explore from the main vector. Leave blank for all
#' @import magrittr rvest dplyr lubridate stringr
#' @export articles_info
#' @return A data frame.
#' @examples
#' url<-c("https://www.mdpi.com/2073-4336/8/4/45","https://www.mdpi.com/2073-4336/11/3/39")
#' \dontrun{
#' articles_info(url, 1.5)
#' }


articles_info <- function(vector,sleep=2,sample_size) {

  if (missing(sample_size)) {
    sample_size=length(vector)
  }else{
    sample_size=sample_size
  }
  
  papers<-sample(vector,sample_size)
  
  pb <- txtProgressBar(min = 0, max = length(papers), initial = 0,style=3) #Build progress bar
  count<-0
  paper_data<-data.frame() #Empty data frame
  
  for (i in papers) {
    
    
    tryCatch(expr={
      paper<-read_html(i)
      
      ex_paper<-paper%>% #obtain editorial times
        html_nodes(".pubhistory")%>%
        html_text2()
      
      ex_paper2<-paper%>% #obtain type of issue
        html_nodes(".belongsTo")%>%
        html_text2()
      
      if (identical( ex_paper,character(0))) {
        ex_paper<-"no"
      } else {
        ex_paper<- ex_paper}
      
      if (identical( ex_paper2,character(0))) {
        ex_paper2<-"no"
      } else {
        ex_paper2<- ex_paper2}
      
      
      
      article_type<-paper%>% # Type of article
        html_nodes(".articletype")%>%
        html_text2()
      
      if (identical( article_type,character(0))) {
        article_type<-"no"
      } else {
        article_type<- article_type}
     
      
    },
    error=function(e){
      ex_paper<<-"error"
      ex_paper2<<-"error"
      article_type<<-"error"
      
      count<-count+1
      setTxtProgressBar(pb, count)
      
    })
    
    temp_df<-data.frame(i,ex_paper,ex_paper2,article_type)
    paper_data<-bind_rows(paper_data,temp_df)
    
    count<-count+1
    Sys.sleep(sleep)
    setTxtProgressBar(pb, count)
    
  }
  final_table<-paper_data%>%
    mutate(Received=gsub("/.*","",ex_paper), #Extract received data time and transform into date
           Received=gsub(".*Received:","",Received),
           Received=as.Date(Received,"%d %B %Y"))%>%
    mutate(Accepted=gsub(".*Accepted:","",ex_paper), #Extract accepted time data and transform into date
           Accepted=gsub("/.*","",Accepted),
           Accepted=as.Date(Accepted,"%d %B %Y"))%>%
    mutate(tat=Accepted-Received, #Calculate turnaround times and add year of acceptance column
           year=year(Accepted))%>%
    mutate(issue_type=case_when(grepl("Section",ex_paper2)~"Section", #Classify articles by issue type
                                grepl("Special Issue",ex_paper2)~"Special Issue",
                                grepl("Topic",ex_paper2)~"Topic",
                                .default = "No"
    ))%>%
    select(-ex_paper,-ex_paper2)
  
  final_table
}
