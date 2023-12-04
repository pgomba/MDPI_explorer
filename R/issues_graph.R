#' Creates a plot showing types of issues were articles where published
#' @param articles_info Output dataframe from function articles_info.
#' @param journal A string with the name of the journal for graph title purposes
#' @import magrittr ggplot2 dplyr lubridate stringr scales
#' @export issues_graph
#' @return A plot
#' @examples
#' issues_graph(agriculture,"Agriculture")

issues_graph<-function(articles_info,journal){
  
  data<-articles_info
  
  issues_table<-data%>%
    group_by(year = floor_date(Accepted,"year"),issue_type)%>%
    summarise(n=n())
  
  graph<-ggplot(issues_table, aes(x=year, y=n,fill=issue_type))+
    scale_fill_brewer(palette = "Set1")+
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
          legend.position = c(0.2, 0.8),
          legend.background = element_rect(fill = "#272822"))+
    labs(fill="Is part of a special issue?",x="Published", y="Number of publications", title = paste0("MDPI ",str_to_title(journal),". Issues"))
  
  graph
  
}

 