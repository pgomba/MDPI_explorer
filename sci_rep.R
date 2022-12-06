library(rvest)
library(tidyverse)
library(scales)

#Create a list of publication paths
#Adapted from suggestion from twitter user @JorritGosens

sitemap<-read_html("https://www.nature.com/sitemap.xml")

papers<-sitemap%>%
  html_nodes("loc")%>%
  html_text2()

#Remove links that are not papers
cleaner<- "srep"
cleaner2<- "vol|jour|coll"
srep<-papers[grep(cleaner, papers)]
srep2<-srep[-grep(cleaner2, srep)]
srep3<-srep[grep("2014", srep)]
srep4<-srep2[46:138]

#pubhistory_srep<-list()

for (t in srep4) {
  #Sys.sleep(60)
  sitemap<-read_html(t)
  papers<-sitemap%>%
    html_nodes("loc")%>%
    html_text2()
  
  for (i in papers) {
    paper<-read_html(i)
    ex_paper<-paper%>%
      html_nodes(".c-bibliographic-information__list")%>%
      html_text2()
    pubhistory_srep<-append(ex_paper,pubhistory_srep)
    
  }
  
}


pub_table<-do.call(rbind, pubhistory_srep)%>%
  as_tibble()%>%
  distinct()%>%
  separate(V1,sep="Accepted:",c("Received","Rest"))%>%
  separate(Rest,sep="Published:",c("Accepted","Rest"))%>%
  separate(Rest,sep="DOI",c("Published","DOI"))%>%
  drop_na()%>% 
  mutate(Received= gsub("Received: ","",Received))%>%
  mutate(Received= lubridate::dmy(gsub(" ","/",Received)))%>%
  mutate(Accepted= lubridate::dmy(gsub(" ","/",Accepted)))%>%
  mutate(Published= lubridate::dmy(gsub(" ","/",Published)))%>%
  mutate(days=Published-Received)%>%
  drop_na()

########

ggplot(pub_table,aes(x=Published,y=0))+
  geom_jitter(alpha=0.2,width = 0, height=2,colour="#d92028")+ 
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
  labs(x="Published", y="", title = paste0("Scientific Reports Dataset - Publications / Time"), caption = paste("Dataset including publications until:",format(max(pub_table$Published),format="%d-%m-%Y")))+
  scale_y_continuous(limits = c(-3,3))+
  geom_text(label=paste("Number of publications:",nrow(pub_table)),x=min(pub_table$Published)+1000,y=-2.5, colour="white", size=5)

ggsave ("average_graph.png", dpi="retina")


average_time_month<-pub_table%>%
  group_by(month = lubridate::floor_date(Published,"month"))%>%
  summarise(average=mean(as.numeric(days)))

ggplot(average_time_month,aes(x=month, y=average))+
  geom_hline(yintercept = mean(as.numeric(pub_table$days)),color="white",linetype = "dashed")+
  geom_point(alpha=1, colour="#d92028",size=2)+ 
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
  labs(x="Published", y="Days", title = paste0("Scientific Reports. Monthly average time between submission and publication"))+
  scale_y_continuous(limits = c(0,300), breaks = seq(0,200,50))+
  geom_text(label=paste("All time average =",round(mean(as.numeric(pub_table$days)),1),"days"), x=min(pub_table$Received)+1000, y=mean(as.numeric(pub_table$days))-20,color="white")

ggsave ("average_graph2.png", dpi="retina")
