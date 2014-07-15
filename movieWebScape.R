IMDb <- function(ID) {
  # Retrieve movie info from IMDb.com. 
  #
  # Args:
  #   ID: IDs of the movies.
  #
  # Returns:
  #   A data frame containing one line per movie, and nine columns: movie ID,
  #   film title, year of release, duration in minutes, MPAA rating, genre(s),
  #   director(s), IMDb rating, and full cast.
  
  # Load required libraries
  require(XML)
  require(pbapply)    # Apply functions with progress bars!!!
  
  # Wrap core of the function in do.call and pblapply in order to
  # pseudo-vectorize it (pblapply) and return a data frame (do.call)
  info <- do.call(rbind, pblapply(ID, FUN = function(ID) {
    # Create movie URL on IMDb.com
    URL <- paste0("http://www.imdb.com/title/tt", ID)
    
    # Download and parse HTML of IMDb page
    parsed.html <- htmlParse(URL)
    
    # Find title
    Film <- xpathSApply(parsed.html, "//h1[@class='header']/span[@class='itemprop']", xmlValue)
    
    # Find year
    Year <- as.numeric(gsub("[^0-9]", "", xpathSApply(parsed.html, "//h1[@class='header']/span[@class='nobr']", xmlValue)))
    
    # Find duration in minutes
    Length_Minutes <- as.numeric(gsub("[^0-9]", "", xpathSApply(parsed.html, "//div[@class='infobar']/time[@itemprop='duration']", xmlValue)))
    
    # Find MPAA rating
    MPAA_Rating <- unname(xpathSApply(parsed.html, "//div[@class='infobar']/span/@content"))
    if (!is.character(MPAA_Rating)) {   # Some movies don't have a MPAA rating
      MPAA_Rating <- "UNRATED"
    }
    
    # Find genre
    Genre <- paste(xpathSApply(parsed.html, "//span[@class='itemprop' and @itemprop='genre']", xmlValue), collapse='|')
    
    # Find director
    Director <- paste(xpathSApply(parsed.html, "//div[@itemprop='director']/a", xmlValue), collapse='|')
    
    # Find IMDB rating
    IMDB_rating <- as.numeric(xpathSApply(parsed.html, "//div[@class='titlePageSprite star-box-giga-star']", xmlValue))
    
    # Extract full cast from the full credits page
    parsed.html <- htmlParse(paste0(URL,"/fullcredits"))
    Full_Cast <- paste(xpathSApply(parsed.html, "//span[@itemprop='name']", xmlValue), collapse='|')
    
    data.frame(ID = ID, Film = Film, Year = Year, Length_Minutes = Length_Minutes,
               MPAA_Rating = MPAA_Rating, Genre = Genre, 
               Director = Director, IMDB_rating = IMDB_rating, Full_Cast = Full_Cast))
  }))
}

# Load data from last challenge 
data <- read.csv("movies-R.csv")

# For each movie, extract IMDb info and append it to the data
data <- within(data, {
  # Extract ID number
  IMDB_ID <- gsub("[^0-9]", "", IMDB_URL)
  
  # Download IMDb info into a temporary variable
  IMDB_Info <- IMDb(IMDB_ID)
  
  # Save MPAA rating
  MPAA_Rating <- IMDB_Info$MPAA_Rating
  
  # Save genre(s)
  Genre <- IMDB_Info$Genre
  
  # Save director(s)
  Director <- IMDB_Info$Director
  
  # Save duration in minutes
  Length_Minutes <- IMDB_Info$Length_Minutes
  
  # Save IMDb rating
  IMDB_rating <- IMDB_Info$IMDB_rating
  
  # Save full cast
  Full_Cast <- IMDB_Info$Full_Cast
  
  # Delete IMDb info
  IMDB_Info <- NULL
})

write.csv(data, file = "movies-R-full.csv")