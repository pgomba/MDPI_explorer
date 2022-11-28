library(rvest)
library(tidyverse)


#Create a list of publication paths

journal<-"Diversity"

links<-list()
for (i in 1:300) {
  temp<-read_html(paste0("https://www.mdpi.com/search?sort=pubdate&page_no=",i,"&page_count=10&year_from=1996&year_to=2022&journal=",journal,"&view=default"))
  extract<- temp%>%
    html_nodes(".title-link") %>% 
    html_attr("href")
  Sys.sleep(1)
  links<-append(extract,links)
}

#Create each publication url and extract editorial data, special issue information
#and calculate day difference between submission and publication

pubhistory<-list()  
for (i in links) {
  Sys.sleep(.5)
  paper<-read_html(paste0("https://www.mdpi.com",i))
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
  drop_na()%>% #remove papers accepted straight away
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
