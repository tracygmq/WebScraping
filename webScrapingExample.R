library(RCurl)

### 1) First task is to get all of the web links we will need ##

base_url<-"http://icml.cc/2013/?page_id=868"
base_html<-getURLContent(base_url)[[1]]#get all links from the base url
links<-strsplit(base_html,"a href=")[[1]]

#links <- paste(links, collapse=" ")
#http://jmlr.org/proceedings/papers/v28/kamyshanska13.html
rule <- "([h][t][t][p].+[.][h][t][m][l])"

require(stringr)

abstract.url <- str_match_all(links,rule) 
abstract.url <- unlist(abstract.url)
#View(abstract.url)

abs.html <- rep(0,length(abstract.url))
for (i in 1:length(abstract.url)){
  abs.html[i] <- getURLContent(abstract.url[i])
}

View(abs.html[120])
