#################################################################################################
####################### Tweets Extraction From Specific Twitter Handles  ########################
#################################################################################################
################# Developer:  Debasish Dutta                                        ############# 
################# Date:       July 2017                                             #############
#################################################################################################
################# Input:      Names of Twitter Handles As Character Vector      #################
#################       	  (Dont Include @ In The Twitter Handle Names)        #################
#################################################################################################

twitter_extraction <- function(twitter_handles) {
  
    ################ Installing & Loading twitteR Package ################
    if (!require("twitteR", character.only = TRUE))
    {
      install.packages("twitteR", dep = TRUE)
      if (!require("twitteR", character.only = TRUE))
        stop("twitteR Package not found")
    }
    
    ################### Twitter API Configuration Keys ###################
    consumer_key <-  "AZ2BABc9masOSebycJfazD3n0"
    consumer_secret <- "xohFvdeHWNhmEQhRaDrHAZZFqxHkfnJ6WZCKbh5dEC859f09Pg"
    access_token <- "4882801050-iQDKpLP0PhT86jQbTT4UK93bDI3oE62sY9RmBFf"
    access_secret <- "LbVCP3zVpFZy30DfdatoazzTuczDL0h4jhOf4WqUMIbRO"
    
    ##################### Accessing The Twitter API #####################
    options(httr_oauth_cache = T)
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    
    ############### Extracting Twitters of Specific Users ###############
    tweet_final <- NULL
    sleepTime = 30
    for (user_name in twitter_handles)
    {
      result <- try(userTimeline(user_name), silent = TRUE)
     
      if (class(result) == "try-error")
        next
      tweet_temp_1 <- userTimeline(user_name, n=3200, includeRts=TRUE, excludeReplies=FALSE)
      tweet_temp_2 <- twListToDF(tweet_temp_1)
      tweet_final <- rbind(tweet_final, tweet_temp_2)
      if(user_name != twitter_handles[length(twitter_handles)]){
      print('Sleeping For 30 Seconds To Avoid Exceeding The Twitter API Request Limit')
      Sys.sleep(sleepTime)
      }
    }
    
    ############### Cleaning The Text ###############
    tweet_final$text <- iconv(tweet_final$text, "latin1", "ASCII", sub="")
    tweet_final$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_final$text) 
    tweet_final$text = gsub("@\\w+", "", tweet_final$text)    
    tweet_final$text = gsub("[[:punct:]]", "", tweet_final$text) 
    tweet_final$text = gsub("[[:digit:]]", "", tweet_final$text)
    tweet_final$text = gsub('[[:cntrl:]]', '', tweet_final$text)
    tweet_final$text = gsub("http\\w+", "", tweet_final$text) 
    tweet_final$text = gsub('\\d+', '', tweet_final$text)
    tweet_final$text = gsub("[ \t]{2,}", "", tweet_final$text) 
    tweet_final$text = gsub("^\\s+|\\s+$", "", tweet_final$text) 
    
    tweet_final <- tweet_final[,c("text", "created","screenName")]
    names(tweet_final) <- c("Tweet_Text", "Tweet_Date", "Twitter_Handle")
    return(tweet_final)
  }
