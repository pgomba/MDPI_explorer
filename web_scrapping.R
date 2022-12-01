library(rvest)
library(tidyverse)

#Create a list of publication paths
#Adapted from suggestion from twitter user @JorritGosens

journal<-"Sustainability"

sitemap<-read_html(paste0("https://www.mdpi.com/sitemap/sitemap.",journal,".xml"))

papers<-sitemap%>%
  html_nodes("loc")%>%
  html_text2()

    #Remove links that are not papers
cleaner<- "guide|even|topi|soci|subm|conf|section|issue|about|announcements|awa|indexing|instructions|apc|history|detailed_instructions|edit|imprint|toc-alert|stats|most_cited"
clean_papers<-papers[-grep(cleaner, papers)]

#Create each publication url and extract editorial data, special issue information
#and calculate day difference between submission and publication

pubhistory<-list()  
for (i in clean_papers) {
  Sys.sleep(1.5)
  paper<-read_html(i)
  ex_paper<-paper%>%
    html_nodes(".pubhistory")%>%
    html_text2()
  ex_paper2<-paper%>%
    html_nodes(".belongsTo")%>%
    html_text2()
  w<-paste(i,"-",ex_paper,"-",ex_paper2)
  pubhistory<-append(w,pubhistory)
  
}

#Create output table
##Disclaimer: Papers accepted straight away (no revision date) removed for simplicity 

pub_table<-do.call(rbind, pubhistory)%>%
  as_tibble()%>%
  separate(V1,sep=" - ",c("link","Publication","Special_issue"))%>%
  separate(Publication,sep="/",c("Received","Revised","Accepted","Published"))%>%
  drop_na()%>%#remove papers accepted straight away
  distinct()%>% #important, when tasks are performed in different days
  mutate(Received= gsub("Received: ","",Received))%>%
  mutate(Received= lubridate::dmy(gsub(" ","/",Received)))%>%
  mutate(Revised=gsub("Revised: ","",Revised))%>%
  mutate(Revised= lubridate::dmy(gsub(" ","/",Revised)))%>%
  mutate(Accepted=gsub("Accepted: ","",Accepted))%>%
  mutate(Accepted= lubridate::dmy(gsub(" ","/",Accepted)))%>%
  mutate(Published=gsub("Published: ","",Published))%>%
  mutate(Published= lubridate::dmy(gsub(" ","/",Published)))%>%
  mutate(days=Published-Received)%>%
  mutate(is_s_issue=if_else(Special_issue=="","No","Yes"))

write.csv(pub_table, paste0("output/",journal,"/pub_table.csv"))

