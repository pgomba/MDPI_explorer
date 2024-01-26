#' Homogenize editors and authors names
#' 
#' Takes a vector of names to return the names without abbreviated middle names, academic titles and hyphens. 
#' 
#' @param name_vector A string with names separated by commas
#' @import magrittr dplyr stringr
#' @export clean_names
#' @return A vector
#' @examples
#' clean_names(c("Matthias M. Bauer","Thomas Garca Morrison","Wolfgang Nitsche", "Elias Biobaca L." ))


clean_names <- function(name_vector) {
    
    cleaned_names<-name_vector%>%
      tolower()%>% #all letter to lower case
      gsub("dr. |prof. |prof. dr. |assoc. prof. | \\u2020","",.) %>% #remove titles and symbols, including ASCII code for †
      gsub("\\b\\w\\.\\s*", "", .)%>%  #remove abbreviated middle names
      word(start = 1,end = 2)%>% #Keep two first items of names to avoid problems with second surname
      str_squish() #remove accidental white spaces
    
    cleaned_names

}

