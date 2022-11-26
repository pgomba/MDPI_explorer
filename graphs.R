library(tidyverse)
library(scales)

#Summary of dataset

summary_graph<-ggplot(pub_table,aes(x=Published,y=0))+
  geom_jitter(alpha=0.5,width = 0, height=2,colour="#779740")+ 
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
  labs(x="Published", y="", title = "MDPI Plants Dataset - Publications / Time", caption = paste("Dataset including publications until:",format(max(pub_table$Published),format="%d-%m-%Y")))+
  scale_y_continuous(limits = c(-3,3))+
  geom_text(label=paste("Number of publications:",nrow(pub_table)),x=min(pub_table$Published)+500,y=-2.5, colour="white", size=5)

ggsave ("summary_graph.tiff",path="output/", dpi="retina")

#Average time between submission to publication by month

average_time_month<-pub_table%>%
  group_by(month = lubridate::floor_date(Published,"month"))%>%
  summarise(average=mean(as.numeric(days)))

ggplot(average_time_month,aes(x=month, y=average))+
  geom_hline(yintercept = mean(as.numeric(pub_table$days)),color="white",linetype = "dashed")+
  geom_point(alpha=1, colour="#779740",size=2)+ 
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
  labs(x="Published", y="Days", title = "MDPI Plants. Monthly average time between submission and publication")+
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))+
  geom_text(label=paste("All time average =",round(mean(as.numeric(pub_table$days)),1),"days"), x=min(pub_table$Received)+400, y=mean(as.numeric(pub_table$days))-10,color="white")

ggsave ("average_graph.tiff",path="output/", dpi="retina")

#Special issues proportion

pub_table2<-pub_table%>%
  group_by(month = lubridate::floor_date(Published,"month"),Special_issue)%>%
  summarise(n=n())

ggplot(pub_table2, aes(x=month, y=n,fill=Special_issue))+
  scale_fill_manual(values=c("red","#779740"))+
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
  labs(fill="Is part of a special issue?",x="Published", y="Number of publications", title = "MDPI Plants. Special issues publications")
  
ggsave ("proportion_graph.tiff",path="output/", dpi="retina")
