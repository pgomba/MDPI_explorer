library(tidyverse)
library(scales)

#Summary of dataset

ggplot(final_table,aes(x=Accepted,y=0))+
  geom_jitter(alpha=0.5,width = 0, height=2,colour="#daeaf5")+ 
  scale_x_date(date_labels = "%Y",breaks = date_breaks("year"))+
  theme_classic()+
  theme(text=element_text(size=20),
        plot.title = element_text(size=14))+
  theme(axis.line.x = element_line(color="white",size = 2),
        axis.line.y = element_blank(),
        axis.text.x = element_text(color="white"),
        axis.text.y = element_blank(),
        legend.position = "none",
        text=element_text(size=16,color = "white"),
        panel.background = element_rect(fill = "#272822"),
        plot.background = element_rect(fill = "#272822"))+
  labs(x="Accepted", y="", title = paste0("MDPI ",str_to_title(journal)," Dataset - Accepted times"),
       caption=paste("Number of publications:",nrow(final_table)))+
  scale_y_continuous(limits = c(-3,3))

ggsave(paste0(journal,"_summary_graph.png"),path=paste0("output/"), dpi="retina")

#Average time between submission to publication by month

average_time_month<-final_table%>%
  group_by(month = floor_date(Accepted, unit = "month"))%>%
  summarise(mean=mean(tat))
  

mean_tat<-final_table%>%
  drop_na(tat)

ggplot(average_time_month,aes(x=month, y=mean))+
  geom_point()+
  geom_hline(yintercept = mean(mean_tat$tat),color="white",linetype = "dashed")+
  geom_point(alpha=1, colour="#daeaf5",size=2)+ 
  scale_x_date(date_labels = "%Y",breaks = date_breaks("year"))+
  theme_classic()+
  theme(text=element_text(size=20),
        plot.title = element_text(size=14))+
  theme(axis.line = element_line(color="white",size = 2),
        axis.text = element_text(color="white"),
        legend.position = "none",
        text=element_text(size=16,color = "white"),
        panel.background = element_rect(fill = "#272822"),
        plot.background = element_rect(fill = "#272822"))+
  labs(x="Accepted", y="Days", title = paste0("MDPI ",str_to_title(journal),". Monthly average time between submission and acceptance"),
       caption=paste("--- All time average =",round(mean(mean_tat$tat),1),"days"),color="white")

ggsave(paste0(journal,"_average_graph.png"),path=paste0("output/"), dpi="retina")

#Special issues proportion

issues_table<-final_table%>%
  group_by(year = floor_date(Accepted,"year"),issue_type)%>%
  summarise(n=n())

ggplot(issues_table, aes(x=year, y=n,fill=issue_type))+
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
  
ggsave(paste0(journal,"_issues_graph.png"),path=paste0("output/"), dpi="retina")

#Article type

article_type<-final_table%>%
  group_by(year = floor_date(Accepted,"year"),article_type)%>%
  summarise(n=n())

ggplot(article_type, aes(x=year, y=n,fill=article_type))+
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

ggsave(paste0(journal,"_types_graph.png"),path=paste0("output/"), dpi="retina")
