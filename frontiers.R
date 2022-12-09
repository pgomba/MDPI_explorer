library(rvest)
library(tidyverse)
library(scales)

sitemap<-read_html("https://www.frontiersin.org/articles/sitemap-index.xml")

links<-sitemap%>%
  html_nodes("loc")%>%
  html_text2()

article_links_1 <- links[grep("articles",links)]
article_links_2 <- links[-grep("images|subjects",links)]

articles<-list()
for (i in article_links_2) {
  sitemap_article<-read_html(i)
  papers<-sitemap_article%>%
  html_nodes("loc")%>%
  html_text2()
articles<-append(papers,articles)
}

pub_table<-do.call(rbind, articles)%>%
  as_tibble()%>%
  distinct()%>%
  separate(V1,sep="/",c("1","2","3","4","5","jrnal"), remove=FALSE)%>%
  separate(jrnal,c("journal","rest","other","next"), remove=FALSE)%>%
  select(V1, journal)

write.csv(pub_table,"output/frontiers/frontiers.csv")

## Test for Frontiers in Plant science

plants_frontiers<-frontiers%>%
  filter(journal=="fpls")

pubhistory_plants_frontiers<-list()
  for (i in pf2$V1) {
    paper<-read_html(i)
    ex_paper<-paper %>%
      html_nodes("#timestamps")%>%
      html_text2()
    w<-paste(i,"-",ex_paper)
    pubhistory_plants_frontiers<-append(w,pubhistory_plants_frontiers)
    
  }

pub_table<-do.call(rbind, pubhistory_plants_frontiers)%>%
  as_tibble()%>%
  separate(V1,sep=" - ",c("DOI","rest"))%>%
  separate(rest,sep=";",c("Received","Accepted","Published"))%>%
  drop_na()%>%#remove papers accepted straight away
  distinct()%>% #important, when tasks are performed in different days
  mutate(Received= gsub("Received: ","",Received))%>%
  mutate(Received= lubridate::dmy(gsub(" ","/",Received)))%>%
  mutate(Accepted=gsub("Accepted: ","",Accepted))%>%
  mutate(Accepted= lubridate::dmy(gsub(" ","/",Accepted)))%>%
  mutate(Published=gsub("Published: ","",Published))%>%
  mutate(Published= lubridate::dmy(gsub(" ","/",Published)))%>%
  mutate(days=Published-Received)%>%
  drop_na()
 

ggplot(pub_table,aes(x=Published,y=0))+
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
  labs(x="Published", y="", title = paste0("Frontiers - Plant Science. Dataset - Publications / Time"), caption = paste("Dataset including publications until:",format(max(pub_table$Published),format="%d-%m-%Y")))+
  scale_y_continuous(limits = c(-3,3))+
  geom_text(label=paste("Number of publications:",nrow(pub_table)),x=min(pub_table$Published)+900,y=-2.5, colour="white", size=5)

average_time_month<-pub_table%>%
  group_by(month = lubridate::floor_date(Published,"month"))%>%
  summarise(average=mean(as.numeric(days)))

ggplot(average_time_month,aes(x=month, y=average))+
  geom_hline(yintercept = mean(as.numeric(pub_table$days)),color="white",linetype = "dashed")+
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
  labs(x="Published", y="Days", title = paste0("Frontiers - Plant Science. Monthly average time between submission and publication"))+
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))+
  geom_text(label=paste("All time average =",round(mean(as.numeric(pub_table$days)),1),"days"), x=min(pub_table$Received)+1000, y=mean(as.numeric(pub_table$days))+20,color="white")

ggsave ("average_graph.png",path=paste0("output/",journal), dpi="retina")