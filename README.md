# R_Programming_Social_Media_Analytics
Text Mining &amp; Sentiment Analysis In Social Media (Facebook, Twitter, Youtube)

Disclaimer: 
This code snippet is developed with an intension for generic use only. I hereby declare that the code was not part of any of my professional development work. Also, no sensitive or confidential data sources are used for this development. 

Description: 
The repository consists of automated R codes for Sentiment Analysis using Social Media data from Facebook or Twitter. There are four user defined functions in this module. Separate user defined functions are available for mining textual data from Twitter and Facebook. Once textual data is extracted then user can perform Sentiment Analysis using any of Sentiment Analysis user defined functions. 

Note: 
1. If user want to mine textual information from any Facebook page (not any user page or Group page, i.e. https://www.facebook.com/xxxx/) then the user must use twitter_extraction function. This function takes only one argument: URLs of Facebook Pages As Character Vector, Facebook Authentication Object Name As Character, Max Count Per Post. Please create your Facebook authentication file before you proceed with the code. This file needs to be present in your current working directory of R Session. 

2. If user want to mine textual information from any Twitter handles then user must use facebook_extraction function. This function takes three arguments: Names of Twitter Handles As Character Vector (without @). Please set up your twitter consumer and access credentials before you proceed with the code. You will be requiring below four parameters: 

      consumer_key <- "aaaa" 
  
      consumer_secret <- "bbbb" 
  
      access_token <- "cccc" 
  
      access_secret <- "dddd" 
  
Kindly supply your arguments in Twitter Extraction.R code (line no 22-25). 
  
  3. There are two approaches of Sentiment Analysis included in this package. One is Non-Dictionary Approach using syuzhet package. This is more robust approach as it classify every sentence in to a set of emotion along with overall sentiment score. User must use sentiment_scoring function for this. This function takes the data frame which contains textual extracts from Facebook/ Twitter and column name of the data frame where textual information is stored. 
  
4. The other approach is Dictionary approach. User must use score_sentiment function which takes textual extracts as character vector, positive and negative word dictionary. 


Steps For Execution: 
1. Copy these code files to the current working directory of R session. 
2. Copy your Facebook authentication file to the current working directory of R session. 
3. Copy Positive and Negative word dictionary in to the current working directory of R session. 
4. Load these files using following commands: 


    source("Facebook Data Extraction.R") 

    source(“Twitter Extraction.R”) 

    source(“Sentiment Analysis_Dictionary Approach.R”) 

    source(“Sentiment Analytics_Non Dictionary Approach.R”)

5. Execute each user defined functions following the input parameters stated above. 

Compatibility: 
The code is developed and tested on RStudio (Version 1.0.44) using R-3.3.2
