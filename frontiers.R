library(rvest)
library(tidyverse)

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

## Test for Frontiers in Plant science

plants_frontiers<-pub_table%>%
  filter(journal=="fpls")

pubhistory_plants_frontiers<-list()
  for (i in plants_frontiers$V1) {
    paper<-read_html(i)
    ex_paper<-paper %>%
      html_nodes("#timestamps")%>%
      html_text2()
    w<-paste(i,"-",ex_paper)
    pubhistory_plants_frontiers<-append(w,pubhistory_plants_frontiers)
    
  }
  

