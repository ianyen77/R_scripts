#install.packages("rvest")
library(rvest)
library(openxlsx)
data<-read.xlsx("C:/Users/USER/Desktop/活頁簿1.xlsx",sheet=1,colNames = F)
genera<-data$X2
get_phylum <- function(genus) {
  url <- paste0("https://en.wikipedia.org/wiki/", genus)
  
  # Get Phylumn in wiki
  page <- try(read_html(url))
  if ('try-error' %in% class(page)) {
    return("Unfind")
  }
  else{
    phylum<-page %>%
      html_nodes(".biota tr td:contains('Phylum') + td a") %>%
      html_text() %>%
      trimws()
    closeAllConnections()
    if(length(phylum) == 0L){
      return("Unfind") 
    }
    else{
      return(phylum)
    }
  }
}

phyla <- c()
x=0
for (g in genera) {
  phyla <- c(phyla, get_phylum(g))
  x<-x+1
  print(x)
}

#combined
data1<-cbind(genera, phyla)
