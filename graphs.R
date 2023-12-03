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
