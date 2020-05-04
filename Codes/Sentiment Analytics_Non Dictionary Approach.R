#################################################################################################
############################### Sentiment Scoring of Sentences  #################################
#################################################################################################
################# Developer:  Debasish Dutta                                        ############# 
################# Date:       July 2017                                             #############
#################################################################################################
################# Input:      1. Name of Data Frame                             #################
#################             2. Column Name Of The Textual Verbatim            #################
#################################################################################################

sentiment_scoring <- function(df_name, text_col_name){
  
################ Installing & Loading twitteR Package ################
if (!require("syuzhet", character.only = TRUE))
{
  install.packages("syuzhet", dep = TRUE)
  if (!require("syuzhet", character.only = TRUE))
    stop("syuzhet Package not found")
}

################ Getting The Sentiment Score of Each Sentences ################
score_syuzhet <- get_sentiment(df_name[,text_col_name], method="syuzhet")
score_bing <- get_sentiment(df_name[,text_col_name], method="bing")
score_afinn <- get_sentiment(df_name[,text_col_name], method="afinn")
score_nrc <- get_sentiment(df_name[,text_col_name], method="nrc")
emotions <- get_nrc_sentiment(df_name[,text_col_name])
names(emotions) <- paste("NRC_", names(emotions), sep = "")

################ Consolidating The Final Result ################
final_result <- cbind(df_name, 
               Syuzhet_Score = score_syuzhet, 
               Bing_Score= score_bing,
               Afinn_Score = score_afinn,
               emotions,
               NRC_Score = score_nrc)

return(final_result)
}
