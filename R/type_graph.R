#' Creates a plot showing a summary of article types
#' @param articles_info Output dataframe from function articles_info.
#' @param journal A string with the name of the journal for graph title purposes
#' @import magrittr ggplot2 dplyr lubridate stringr scales tidyr
#' @export types_graph
#' @return A plot
#' @examples
#' types_graph(agriculture,"Agriculture")

types_graph<-function(articles_info,journal){
  
  data<-articles_info
  
  article_type<-data%>%
    group_by(year = floor_date(Accepted,"year"),article_type)%>%
    summarise(n=n())
  
  graph<-ggplot(article_type, aes(x=year, y=n,fill=article_type))+
    geom_col()+ 
    scale_x_date(date_labels = "%Y",breaks = date_breaks("year"))+
    theme_classic()+
    theme(text=element_text(size=20),
          plot.title = element_text(size=14))+
    theme(axis.line = element_line(color="white",size = 2),
          axis.text = element_text(color="white"),
          text=element_text(size=16,color = "white"),
          panel.background = element_rect(fill = "#272822"),
          plot.background = element_rect(fill = "#272822"),
          legend.position = c(0.25, 0.60),
          legend.background = element_rect(fill = "#272822"))+
    labs(fill="Type",x="Accepted", y="Number of publications", title = paste0("MDPI ",str_to_title(journal),". Article types"))
  
  graph
  
}
