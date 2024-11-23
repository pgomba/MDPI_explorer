#' This function extracts key editorial information from one or more paper URLs. Specifically, it retrieves the submission, revision, and acceptance dates, as well as the article type. The function also calculates the turnaround time (the duration from submission to acceptance) and identifies whether the paper is part of a special issue.
#' @param vector A vector with urls.
#' @param sleep Number of seconds between scraping iterations. 2 sec. by default
#' @param sample_size A number. How many papers do you want to explore from the main vector. Leave blank for all
#' @param show_progress Logical. If `TRUE`, a progress bar is displayed during the function execution. Defaults to `TRUE`.
#' @import magrittr rvest dplyr lubridate stringr
#' @export article_info
#' @return A data frame (class: \code{data.frame}) with the following columns:
#' \describe{
#'   \item{i}{The URL of the article from which the information is retrieved.}
#'   \item{article_type}{The classification of the article (e.g., editorial, review).}
#'   \item{Received}{The date the article was received by the publisher.}
#'   \item{Revised}{The date the article was confirmed as revised by the publisher.}
#'   \item{Accepted}{The date the article was accepted for publication.}
#'   \item{tat}{The turnaround time, calculated as the number of days between the received and accepted dates.}
#'   \item{year}{The year in which the article was accepted for publication.}
#'   \item{issue_type}{Indicates whether the article is part of a special issue.}
#'   \item{open_peer_review}{Indictes if article peer review is publicly available or not}
#' }
#' @examples
#' url<-c("https://www.mdpi.com/2073-4336/8/4/45","https://www.mdpi.com/2073-4336/11/3/39")
#' \donttest{
#' info<-article_info(url, 1.5)
#' }
#' 


article_info <- function(vector,sleep=2,sample_size,show_progress=TRUE) {

  if (missing(sample_size)) {
    sample_size=length(vector)
  }else{
    sample_size=sample_size
  }
  
  papers<-sample(vector,sample_size)
  
  if (show_progress) {
  pb <- txtProgressBar(min = 0, max = length(papers), initial = 0,style=3) #Build progress bar
  }
  
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
      
      open_review<-paper%>%
        html_nodes(".download .button--color-inversed")%>%
        html_text2()
      
      open_peer_review <- ifelse(any(grepl("Review Reports", open_review, ignore.case = TRUE)), "Yes", "No")
      
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
      
      if (show_progress) {
      
      count<-count+1
      setTxtProgressBar(pb, count)
      }
      
    })
    
    temp_df<-data.frame(i,ex_paper,ex_paper2,article_type,open_peer_review)
    paper_data<-bind_rows(paper_data,temp_df)
    
    count<-count+1
    Sys.sleep(sleep)
    
    if (show_progress) {
    setTxtProgressBar(pb, count)
    }
    
  }
  
  final_table<-paper_data%>%
    mutate(Received=gsub("/.*","",tolower(ex_paper)), #Extract received data time and transform into date
           Received=gsub(".*received:","",Received),
           Received=as.Date(Received,"%d %B %Y"))%>%
    mutate(Revised=str_extract(ex_paper, "(?<=Revised: )[0-9]{1,2} \\w+ \\d{4}"),
           Revised=as.Date(Revised,"%d %B %Y"))%>%
    mutate(Accepted=gsub(".*accepted:","",tolower(ex_paper)), #Extract accepted time data and transform into date
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
