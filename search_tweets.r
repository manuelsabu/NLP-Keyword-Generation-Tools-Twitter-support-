# Libraries for this file                 # Functions in this file
#_______________________                  #_______________________
library(twitteR)                          # SearchTweets()
library(ROAuth)                           # ScanTwitterHandles
#_______________________                  #_______________________
# Set your path here 
# Path should be the path upto to folder which contains the required files
# The path should not contain the trailing slash ( "/" )
path <- "C:/Users/sudev.ac/Desktop/Code/R/trigger"

path.twitter.credentials <- paste(path,'/twitteR_credentials.Rdata', sep='')
load(path.twitter.credentials)

# Registers the Twitter OAuth.
# Related files: register.r | Autherizatioin.r
registerTwitterOAuth(twitCred)

#######---------------------------  ScanTwitterHandles()  ---------------------------#######

# Purpose: Scans twitter handles present in "TwitterHandles.txt" by default

# Arguments: (Void)

# Returns: Character Vector

ScanTwitterHandles <- function(file.Name = "TwitterHandles.txt")
{
      twitter.Handles <- scan(file = file.Name, what = character(), 
                              sep = " ", comment.char = "#")
}
#######------------------------------------------------------------------------------#######

#######------------------------------  SearchTweets()  ------------------------------#######

# Purpose: Searches the twitter API according to the given configurations

# Arguments: (Character, ...)

# Returns: Listof Character Vectors


SearchTweets <- function(search.Key, is.public.Tweets = TRUE, max.Pub.Tweets = 1000, 
                         max.Pri.Tweets = 20)   # More arguments can be added
{
      search.Results <- 0
      
      if(is.public.Tweets)
      {
            # Search results from searchTwitter returned as "status" class.
            search.Results <- searchTwitter(search.Key, n = max.Pub.Tweets, 
                                            cainfo = "cacert.pem", lang = "en")
            
            # Converts search.Results into a dataframe.
            search.Results.DF <- twListToDF(search.Results)
            
            # Converts the dates in the dataframe to "Date" format in UTC timezone.
            search.Results.DF$created <- as.Date(search.Results.DF$created)
            path.raw.csv <- paste(path,'/raw_twitter_data.csv', sep='')
            write.csv(search.Results.DF, 
                      path.raw.csv,
                      fileEncoding = "UTF-8")
            
            search.Results <- search.Results.DF
      }
      
      else
      {
            # Stores all the twitter handles from file.
            twitter.Handles <- ScanTwitterHandles()
            no.Of.Handles <- length(twitter.Handles)
            
            # If no twitter handles are in the file
            if(0 == no.Of.Handles)
            {
                  print("No handles were given. Please check the file:
                        TwitterHandles.txt or the function scan_twitter_handles.R")
                  
                  break
            }
            else
            {
                  # Loop to do the search on every twitter handle from file.
                  for(i in 1:no.Of.Handles)
                  {
                        # Iteration status print
                        print(sprintf("Searching Handle Number %d Out Of %d", i,no.Of.Handles)) 
                        flush.console()  
                        
                        # For the first iteration
                        if(1 == i)
                        {
                              search.Results <- searchTwitter(paste(search.Key, " from:", 
                                                                    twitter.Handles[i]), 
                                                              n = max.Pri.Tweets, cainfo = "cacert.pem", 
                                                              lang = "en")
                        }
                        
                        else
                        {
                              search.Results <- c(search.Results, 
                                                 searchTwitter(paste(search.Key, " from:", 
                                                                     twitter.Handles[i]), 
                                                               n = max.Pri.Tweets, cainfo = "cacert.pem", 
                                                               lang = "en")) 
                        }
                  }
            }
      }  
      search.Results 
}
#########--------------------------------------------------------------------------#########
