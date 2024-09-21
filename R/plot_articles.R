#' Plots informaton obtained from article_info
#' 
#' @param articles_info Output dataframe from function articles_info.
#' @param journal A string with the name of the journal for graph title purposes
#' @param type select "summary","issues" or "type" depending on desired graph
#' @import magrittr ggplot2 dplyr lubridate stringr scales
#' @export plot_articles
#' @return A plot
#' @examples
#' plot_articles(agriculture,"Agriculture",type="summary")
#' 

plot_articles<-function(articles_info,journal,type){
  
  switch(type,
         "summary" = {
           
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
           
           
         },
         "issues" = {
           
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
                   legend.position = "bottom",
                   legend.background = element_rect(fill = "#272822"))+
             labs(fill="Is part of a special issue?",x="Accepted", y="Number of publications", title = paste0("MDPI ",str_to_title(journal),". Issues"))
           
           
           
           
         },
         "type" = {
           
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
             theme(axis.line = element_line(color="white",linewidth = 2),
                   axis.text = element_text(color="white"),
                   text=element_text(size=16,color = "white"),
                   panel.background = element_rect(fill = "#272822"),
                   plot.background = element_rect(fill = "#272822"),
                   legend.position = "bottom",
                   legend.background = element_rect(fill = "#272822"))+
             labs(fill="Type",x="Accepted", y="Number of publications", title = paste0("MDPI ",str_to_title(journal),". Article types"))
           
           
           
           
         },
         {
           message("type must be 'summary, 'issues' or 'type'")
         }
  )
  
  graph
  
}
