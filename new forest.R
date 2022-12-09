library(rvest)
library(tidyverse)
library(scales)

sitemap<-read_html("https://link.springer.com/sitemap-index.xml")

links<-sitemap%>%
  html_nodes("loc")%>%
  html_text2()

#remove corrupted links

articles_spring<-list()
for (i in links) {
  sitemap_article<- read_html(i)%>%
    try(html_text(),silent = TRUE)
art<-stringr::str_split(sitemap_article,pattern = "\n")
art2<-unlist(art)
  articles_spring<-append(art2,articles_spring)
}

springer_df<-as.data.frame(unlist(articles_spring))
new_forest<- springer_df %>%
  filter(str_detect(`unlist(articles_spring)`, "11056"))

nf_articles<-list()
for (i in new_forest$`unlist(articles_spring)`) {
  Sys.sleep(.2)
    sitemap_article<- read_html(i)
    
    Editorial<-sitemap_article%>%html_nodes("[class='c-bibliographic-information__list-item']")%>%
      html_text2()%>%
      paste(sep = " ", collapse = " ")
    
    DOI<-sitemap_article%>%
      html_nodes("[class='c-bibliographic-information__list-item c-bibliographic-information__list-item--doi']")%>%
      html_text2()
    
    abstract<-sitemap_article%>%html_nodes("#Abs1-content")%>%html_text2()
    
    title<-sitemap_article%>%html_nodes(".c-article-title")%>%html_text2()
    
    keywords<- sitemap_article%>%
      html_nodes("[class='c-article-subject-list']")%>%
      html_text2()
  
  data_col<- paste(title,DOI,Editorial,abstract,keywords,sep = "@@@")
  
nf_articles<- append(data_col,nf_articles)
    
}

nf_table<-do.call(rbind, nf_articles)
write.csv(nf_table,"nf_table.csv")

nf_table_clean<-nf_table%>%
  as.tibble()%>%
  separate(V1,sep="@@@",c("Title","DOI","Editorial","Abstract"))%>%
  separate(Editorial,sep="TTT",c("Received","Accepted","Published","DOI"))%>%
  filter(!is.na(Abstract))

nf2<-nf_table_clean[grepl("doi.org", nf_table_clean$DOI),]

