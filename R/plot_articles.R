#' Plots information obtained from article_info. For analysis purposes, Editorial and Correction type articles are ignored.
#' 
#' @param articles_info Output dataframe from function articles_info.
#' @param journal A string with the name of the journal for graph title purposes
#' @param type select "summary","issues", "tat", "review" or "type" depending on desired graph
#' @import magrittr ggplot2 dplyr lubridate stringr scales tidyr
#' @export plot_articles
#' @return A plot (class: \code{ggplot}) depicting the desired information obtained from \code{article_info}
#' @examples
#' plot_articles(agriculture,"Agriculture",type="summary")
#' 

plot_articles<-function(articles_info,journal,type){
  
  valid_types <- c("tat", "summary", "issues", "type", "review")
  if (!type %in% valid_types) {
    stop("Error: parameter type must be 'summary, 'issues', 'tat' or 'type'")
  }
  
  switch(type,
         
         "review" = {
           
           open_review<-articles_info%>%
             summarize(.by=c(year, open_peer_review), n=n())%>%
             filter(if_all(everything(), ~ !is.na(.)))%>% #alternative to drop_na()
             complete(year, open_peer_review = c("Yes", "No"), fill = list(n = 0))
           
           
           graph<- ggplot(open_review,aes(x=year,y=n,fill=open_peer_review))+
             geom_col()+
             scale_fill_manual(values = c("#dc0000b2","#3366CC"))+
             theme_classic()+
             theme(text=element_text(size=20),
                   plot.title = element_text(size=14))+
             theme(axis.line.x = element_line(color="white", linewidth =  2),
                   axis.line.y = element_line(color="white", linewidth =  2),
                   axis.text.x = element_text(color="white"),
                   axis.text.y = element_text(color="white"),
                   legend.position = "bottom",
                   legend.background = element_rect(fill = "#272822"),
                   text=element_text(size=16,color = "white"),
                   panel.background = element_rect(fill = "#272822"),
                   plot.background = element_rect(fill = "#272822"))+
             labs(x="Year", y="Articles", title = paste0("MDPI ",str_to_title(journal)," - Proportion of open reviews"),
                  fill="Review available?")+
             scale_y_continuous(limits = c(0,NA))
           
           graph
           
           
         },
         
         
         "tat" = {
           
           tat<-articles_info%>%
             filter(!article_type%in%c("Editorial","Correction"))%>%
             mutate(year_month = floor_date(Accepted, "month")) %>%
             summarize(.by=year_month, mean_tat=mean(tat))%>%
             filter(if_all(everything(), ~ !is.na(.))) #alternative to drop_na()
           
           last_tat<-tat%>%
             filter(year_month==max(year_month))%>%
             .[[2]]
           
           
           graph<- ggplot(tat,aes(x=year_month,y=mean_tat))+
             annotate("segment", x = min(tat$year_month),xend = max(tat$year_month),y = last_tat, 
               yend = last_tat, colour = "red", alpha = 0.7,linetype = "dotted")+
             scale_x_date(date_labels = "%Y",breaks = date_breaks("year"))+
             geom_point(shape=21,colour="white",fill="#daeaf5",size=3)+
             
             scale_x_date(date_labels = "%Y",breaks = date_breaks("year"))+
             theme_classic()+
             geom_line(colour="white",alpha=.2,linetype="dashed")+
             theme(text=element_text(size=20),
                   plot.title = element_text(size=14))+
             theme(axis.line.x = element_line(color="white", linewidth =  2),
                   axis.line.y = element_line(color="white", linewidth =  2),
                   axis.text.x = element_text(color="white"),
                   axis.text.y = element_text(color="white"),
                   legend.position = "none",
                   text=element_text(size=16,color = "white"),
                   panel.background = element_rect(fill = "#272822"),
                   plot.background = element_rect(fill = "#272822"))+
             labs(x="Accepted", y="Days", title = paste0("MDPI ",str_to_title(journal)," - Monthly Turnaround Times"),
                  caption=paste("Last month TAT:",round(last_tat,2), "days (dotted red line)"))+
             scale_y_continuous(limits = c(0,NA))
           
           
         },
         
         
         
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
           
           
           
           
         }
  )
  
  graph
  
}
