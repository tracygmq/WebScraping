library(RCurl)      # Everything necessary to grab webpages on the Web
library(XML)        # Everything necessary to parse XML and HTML code
library(pbapply)    # Progress bars!!! Just because why not :-) 

# Create curl handle which can be used for multiple HHTP requests. 
# followlocation = TRUE in case one of the URLs we want to grab is a redirection
# link.
curl <- getCurlHandle(useragent = "R", followlocation = TRUE)

# Prepare URLs of the movie lists alphabetically ordered by first letter of
# movie title (capital A to Z, except for v and y) + "numbers" list (for movies
# which title starts with a number)
urls.by.letter <- paste0("http://www.moviebodycounts.com/movies-", 
                         c("numbers", LETTERS[1:21], "v", "W" , "x", "Y", "Z"), ".htm")

# For each movie list... For loops are frowned upon in R, let's use the classier
# apply functions instead. Here I use the pblapply from the pbapply package.
# It's equivalent to the regular lapply function, but it provides a neat 
# progress bar. Unlist to get a vector. 
urls.by.movie <- unlist(pblapply(urls.by.letter, FUN = function(URL) {
  # Load raw HTML
  raw.html <- getURL(URL, curl = curl)
  # Parse HTML content
  parsed.html <- htmlParse(raw.html)
  # Extract desired links from HTML content using XPath. 
  # The desired links are all the URLs ("a/@href") directly following
  # ("/following::") the image which source file is called "graphic-movies.jpg" 
  # ("//img[@src='graphic-movies.jpg']").
  links <- as.vector(xpathSApply(parsed.html, "//img[@src='graphic-movies.jpg']/following::a/@href"))
  
  # Most links are relative URLs. Add root of the website to make them absolute.
  if (!is.null(links)) {
    ix =grepl("http://www.moviebodycounts.com/", links)                  # Find relative URLs
    links[!ix] <- paste0("http://www.moviebodycounts.com/", links[!ix])   # Add root of website to make URLs absolute
    return(links)
  }
}), use.names =FALSE) # close the loop

# One URL is actually just a symbolic link to another page. Let's get rid of it.
ix <- which(grepl("movies-C.htm", urls.by.movie))
urls.by.movie <- urls.by.movie[-ix]

# For each movie... 
# do.call(rbind, ...) to reorganize the results in a nice data frame
data <- do.call(rbind, pblapply(urls.by.movie, FUN = function(URL) {
  
  # Load raw HTML
  raw.html <- getURL(URL, curl = curl)
  
  # Parse HTML content
  parsed.html <- htmlParse(raw.html)
  
  # Find movie title
  # Title appears inside a XML/HTML node called "title" ("//title"). In this
  # node, it comes after "Movie Body Counts: ". I use gsub to get rid off "Movie
  # Body Counts: " and keep only the movie title.
  Film <- xpathSApply(parsed.html, "//title", xmlValue)
  Film <- gsub("Movie Body Counts: ", "", Film)
  
  # Find movie year
  # The year is usually a text inside ("/descendant::text()") a link node
  # ("//a") which source contains the string "charts-year" ("[contains(@href,
  # 'charts-year')]").
  Year <-as.numeric(xpathSApply(parsed.html, "//a[contains(@href, 'charts-year')]/descendant::text()", xmlValue))
  
  # Find IMDB link
  # The IMDB link is inside a link node ("//a") which source contains "imdb"
  # ("/@href[contains(.,'imdb')]")
  IMDB_URL <- as.vector(xpathSApply(parsed.html, "//a/@href[contains(.,'imdb')]"))[1]
  
  # Note: We select the first element of the vector because for at least one of
  # the movies, this command returns two links.
  
  # Find kill count.
  # Kill count is contained in the first non-empty text node
  # ("/following::text()[normalize-space()]") after the image which source file
  # is called "graphic-bc.jpg" ("//img[@src='graphic-bc.jpg']")
  Body_Count <- xpathSApply(parsed.html, "//img[@src='graphic-bc.jpg']/following::text()[normalize-space()]", xmlValue)[1]
  
  # Now we need to clean up the text node that we just extracted because there
  # are lots of inconsistencies in the way the kill counts are displayed across
  # all movie pages. For instance, counts are sometimes accompanied by text, not
  # always the same, and sometimes there is no text at all. Sometimes the total
  # count is split in two numbers (e.g., number of dead humans and number of
  # dead aliens). And sometimes the total count is displayed and accompanied by
  # a split count in parenthesis. First, let's remove everything that is
  # writtent in parenthesis or that is not a number.
  # Using gsub, remove everything in parenthesis and all non number characters
  Body_Count <- gsub("\\(.*?\\)", " ", Body_Count)
  Body_Count <- gsub("[^0-9]+", " ", Body_Count)
  
  # In case the total count has been split, we want to separate these numbers
  # from each other so that we can add them up later. Using strsplit, split the
  # character string at spaces
  Body_Count <- unlist(strsplit(Body_Count, " "))
  
  # For now, we have extracted characters. Transform them into numbers.
  Body_Count <- as.numeric(Body_Count)
  
  # Sum up the numbers (in case they have been split into separate categories.
  Body_Count <- sum(Body_Count, na.rm = TRUE)
  
  # Return scraped data into a data frame form
  return(data.frame(IMDB_URL, Film, Year, Body_Count))
})) # close the loop

# Save scraped data in a .csv file for future use
write.csv(data, "movies-R.csv", row.names = FALSE)

