#' Creates a plot with monthly turnaround times using the obtained via articles_info()
#' @param articles_info Output dataframe from function articles_info.
#' @param journal A string with the name of the journal for graph title purposes
#' @import magrittr ggplot2 dplyr lubridate stringr scales 
#' @importFrom tidyr drop_na
#' @export average_graph
#' @return A plot
#' @examples
#' average_graph(agriculture,"Agriculture")


average_graph<-function(articles_info,journal){
  
  
  data<-articles_info
  
  average_time_month<-data%>%
    group_by(month = floor_date(Accepted, unit = "month"))%>%
    summarise(mean=mean(tat))
  
  
  mean_tat<-data%>%
    drop_na(tat)
  
  graph<- ggplot(average_time_month,aes(x=month, y=mean))+
    geom_point()+
    geom_hline(yintercept = mean(mean_tat$tat),color="white",linetype = "dashed")+
    geom_point(alpha=1, colour="#daeaf5",size=2)+ 
    scale_x_date(date_labels = "%Y",breaks = date_breaks("year"))+
    theme_classic()+
    theme(text=element_text(size=20),
          plot.title = element_text(size=14))+
    theme(axis.line = element_line(color="white",linewidth = 2),
          axis.text = element_text(color="white"),
          legend.position = "none",
          text=element_text(size=16,color = "white"),
          panel.background = element_rect(fill = "#272822"),
          plot.background = element_rect(fill = "#272822"))+
    labs(x="Accepted", y="Days", title = paste0("MDPI ",str_to_title(journal),". Monthly average time between submission and acceptance"),
         caption=paste("--- All time average =",round(mean(mean_tat$tat),1),"days"),color="white")
  
  graph
  
}

