#' MDPI journal names and code
#' 
#' Extracts names and codes of current MDPI journals. 
#' 
#' @import magrittr rvest
#' @export MDPI_journals
#' @return A data frame (class: \code{data.frame}) with the following columns:
#' \describe{
#'   \item{journal}{Full name of the MDPI journal}
#'   \item{num_papers}{Journal code used for ID and web scraping purposes}
#' }
#' @examples
#' journal_table<-MDPI_journals()
#' 

MDPI_journals<-function(){
  
  data<-read_html("https://www.mdpi.com/about/journals")
  
  journal<- data%>%html_nodes(".lean div")%>%html_text2()
  code<-  data%>%html_nodes(".lean")%>%html_attr("href")%>%
    gsub("/journal/","",.)
  
  journal_table<-data.frame(journal=journal, code=code)
  
  journal_table
  
}



  
  
