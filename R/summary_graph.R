#' Creates a summary of the dataframe obtained via articles_info()
#' @param articles_info Output dataframe from function articles_info.
#' @param journal A string with the name of the journal for graph title purposes
#' @import magrittr ggplot2 dplyr lubridate stringr scales
#' @export summary_graph
#' @return A plot
#' @examples
#' summary_graph(agriculture,"Agriculture")


summary_graph <- function(articles_info,journal) {
  
  graph<- ggplot(articles_info,aes(x=Accepted,y=0))+
    geom_jitter(alpha=0.5,width = 0, height=2,colour="#daeaf5")+ 
    scale_x_date(date_labels = "%Y",breaks = date_breaks("year"))+
    theme_classic()+
    theme(text=element_text(size=20),
          plot.title = element_text(size=14))+
    theme(axis.line.x = element_line(color="white", linewidth =  2),
          axis.line.y = element_blank(),
          axis.text.x = element_text(color="white"),
          axis.text.y = element_blank(),
          legend.position = "none",
          text=element_text(size=16,color = "white"),
          panel.background = element_rect(fill = "#272822"),
          plot.background = element_rect(fill = "#272822"))+
    labs(x="Accepted", y="", title = paste0("MDPI ",str_to_title(journal)," Dataset - Accepted times"),
         caption=paste("Number of publications:",nrow(articles_info)))+
    scale_y_continuous(limits = c(-3,3))
  
  graph
}
