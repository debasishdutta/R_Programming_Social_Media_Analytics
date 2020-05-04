##################################################
# Social Media  platform : Youtube 
# Extract data for sentiment analysis
##################################################

# Remove all objects from the current workspace (R memory)
rm(list = ls())
print("Successfully - Removed all objects from the current workspace (R memory)")

# Set Variables required for the analysis
Outputdirectory         <-"*************************"
Inputdirectory          <- "*************************"
Modeloutdirectory       <- "**************************"

Youtube_app_id          <-"************************"
Youtube_app_password    <-"************************"
Youtube_searchstring    <-"kurkure"

## To fetch data from 2014-01-01 as we have imagery data from Q1'14 and Sales Volume from Oct of 2014
Youtube_fromdate = "2014-01-01T00:00:00Z"
#Youtube_todate = "2017-08-30T00:00:00Z"

Youtube_viewCount               <-100000

# Set current program Working directory
setwd("*******************************************")
print (paste0("Current Working Directory - ",getwd()))

# Loading all the libraries
# install.packages("devtools")
# devtools::install_github("soodoku/tuber", build_vignettes = TRUE)
library(tuber)  # This is your tube wrapper package for data extraction 
library(lubridate) # This library will support ymd_hms function
library(syuzhet) # This library will support get_sentiment() function.
library(tm) # This library will support Corpus, removeWords and TermDocumentMatrix functions
library(wordcloud) # This library will support comparison.cloud and commonality.cloud
library(MASS)
library(dplyr)
library(plyr);
library(stringr);
library(ggplot2)

# Youtube  : Sets up the OAuth credentials for a Youtube session
yt_oauth(Youtube_app_id,Youtube_app_password)

# Youtube  : Search all videos that contain a certain keyword published after Jan 2014
suppressWarnings(rm(Youtube_all_videos))
Youtube_all_videos<-yt_search(term = Youtube_searchstring, published_after = Youtube_fromdate)
# Returns 567 observations

# Youtube  : Structure of data frame
class(Youtube_all_videos)
str(Youtube_all_videos)
dim(Youtube_all_videos)
summary(Youtube_all_videos)

# Youtube  : Modify Column Name of data frame
Youtube_all_videos$video_id<-as.character(Youtube_all_videos$video_id)

# Youtube  : Extract list of Youtube video stats
suppressWarnings(rm(Findstats))
Findstats<-''
error_output <- " - Video - Not Processed"
cat("\n")
print (paste0("Total Youtube waiting to process - ",length(Youtube_all_videos$video_id)))
for(i in 1:length(Youtube_all_videos$video_id))
{
  cat("ID= ",i,"of",length(Youtube_all_videos$video_id) , "Video id = ", Youtube_all_videos$video_id[i])
  Findstats<-suppressWarnings(rbind(Findstats,tryCatch(get_stats(video_id = Youtube_all_videos$video_id[i]),error=function(e) print(paste0(Youtube_all_videos$video_id[i], error_output)), finally=print(paste0(Youtube_all_videos$video_id[i]," - Video Processed")))))
}
rm(error_output)

suppressWarnings(rm(Youtube_stats))
Youtube_stats <- data.frame()
for(i in 2:nrow(Findstats))
{
    if (as.numeric(Findstats[i,]$viewCount)>=Youtube_viewCount)
    {
      print (paste0("Video Processed - Video ID - ",Findstats[i,]$id," View Count- ",Findstats[i,]$viewCount))
      Youtube_stats<-rbind(Youtube_stats,data.frame(Youtube_video_id=Findstats[i,]$id,
                                                    Youtube_video_viewcount=Findstats[i,]$viewCount,
                                                    Youtube_video_likecount=Findstats[i,]$likeCount,
                                                    Youtube_video_dislikecount=Findstats[i,]$dislikeCount,
                                                    Youtube_video_favoritecount=Findstats[i,]$favoriteCount,
                                                    Youtube_video_commentcount=Findstats[i,]$commentCount))
    }
  else
    {
      print (paste0("Video Skipped - Video ID - ",Findstats[i,]$id," View Count- ",Findstats[i,]$viewCount))
    }
  cat("\n")
  print (paste0("In Progress - Total Video Stats Loaded Successfully till now - ",nrow(Youtube_stats)))
} 
print (paste0("Completed - Total Video Stats Loaded Successfully - ",nrow(Youtube_stats)))

str(Youtube_stats)
# Youtube  : Modify Column Name of data frame
Youtube_stats$Youtube_video_id<-as.character(Youtube_stats$Youtube_video_id)
Youtube_stats$Youtube_video_viewcount<-as.numeric(as.character(Youtube_stats$Youtube_video_viewcount))
Youtube_stats$Youtube_video_likecount<-as.numeric(as.character(Youtube_stats$Youtube_video_likecount))
Youtube_stats$Youtube_video_dislikecount<-as.numeric(as.character(Youtube_stats$Youtube_video_dislikecount))
Youtube_stats$Youtube_video_favoritecount<-as.numeric(as.character(Youtube_stats$Youtube_video_favoritecount))
Youtube_stats$Youtube_video_commentcount<-as.numeric(as.character(Youtube_stats$Youtube_video_commentcount))

Youtube_stats <- Youtube_stats[!is.na(Youtube_stats$Youtube_video_favoritecount),]

# Youtube  : Structure of data frame
str(Youtube_stats)
summary(Youtube_stats)

# Youtube  : Extract User Comments of Youtube videos
suppressWarnings(rm(Youtube_all_comments))
Youtube_all_comments <- data.frame()
error_output <- " - Video - Not Processed"
cat("\n")
print (paste0("Total Youtube waiting to process to extract comments- ",length(Youtube_stats$Youtube_video_id)))
for(i in 1:length(Youtube_stats$Youtube_video_id))
{
  cat("ID= ",i,"of",length(Youtube_stats$Youtube_video_id) , "Video id = ",Youtube_stats$Youtube_video_id[i], "Video Comment Count = ",Youtube_stats$Youtube_video_commentcount[i])
  Youtube_all_comments<-suppressWarnings(rbind(Youtube_all_comments,tryCatch(get_all_comments(video_id = Youtube_stats$Youtube_video_id[i]),error=function(e) print(paste0(Youtube_stats$Youtube_video_id[i], error_output)), finally=print(paste0(Youtube_stats$Youtube_video_id[i]," - Video Processed")))))
  cat("\n")
  print (paste0("In Progress - Total Video Comments Loaded Successfully till now - ",nrow(Youtube_all_comments)))
}
## will fetch 11540 comments for 58 videos.

str(Youtube_all_comments)

# Youtube  : Modify Column Name of data frame
Youtube_all_comments$authorDisplayName<-as.character(Youtube_all_comments$authorDisplayName)
Youtube_all_comments$authorProfileImageUrl<-as.character(Youtube_all_comments$authorProfileImageUrl)
Youtube_all_comments$authorChannelUrl<-as.character(Youtube_all_comments$authorChannelUrl)
Youtube_all_comments$authorChannelId.value<-as.character(Youtube_all_comments$authorChannelId.value)
Youtube_all_comments$videoId<-as.character(Youtube_all_comments$videoId)
Youtube_all_comments$textDisplay<-as.character(Youtube_all_comments$textDisplay)
Youtube_all_comments$textOriginal<-as.character(Youtube_all_comments$textOriginal)
Youtube_all_comments$canRate<-as.character(Youtube_all_comments$canRate)
Youtube_all_comments$viewerRating<-as.character(Youtube_all_comments$viewerRating)
Youtube_all_comments$likeCount<-as.numeric(as.character(Youtube_all_comments$likeCount))
Youtube_all_comments$publishedAt<-as.character(Youtube_all_comments$publishedAt)
Youtube_all_comments$updatedAt<-as.character(Youtube_all_comments$updatedAt)
Youtube_all_comments$id<-as.character(Youtube_all_comments$id)
Youtube_all_comments$parentId<-as.character(Youtube_all_comments$parentId)
Youtube_all_comments$moderationStatus<-as.character(Youtube_all_comments$moderationStatus)

sum(is.na(Youtube_all_comments$likeCount))

# Remove blank comments
Youtube_all_comments <- Youtube_all_comments[!is.na(Youtube_all_comments$likeCount),]
# with this it will be 11523 comments

# Youtube  : Structure of data frame
str(Youtube_all_comments)

# Youtube  : Data Cleaning Function
f_clean_posts <- function (fbposts) 
{
  # remove re entries
  clean_fbposts = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",fbposts)
  # remove at people
  clean_fbposts = gsub("@\\w+", " ", clean_fbposts)
  # remove punctuation
  clean_fbposts = gsub("[[:punct:]]", " ", clean_fbposts)
  clean_fbposts = gsub('[[:cntrl:]]', ' ', clean_fbposts)
  # remove numbers
  clean_fbposts = gsub("[[:digit:]]", " ", clean_fbposts)
  # remove html links
  clean_fbposts = gsub("http\\w+..*", " ", clean_fbposts)
  # remove unnecessary spaces
  clean_fbposts = gsub("[ \t]{2,}", " ", clean_fbposts)
  clean_fbposts = gsub("^\\s+|\\s+$", " ", clean_fbposts)
  # remove emojis or special characters
  clean_fbposts = gsub('<.*>', ' ', enc2native(clean_fbposts))
  #Create Lower Case
  clean_fbposts = tolower(clean_fbposts)
  clean_fbposts = trimws(clean_fbposts)
  clean_fbposts
}

# Youtube  : Calling function to clean the Youtube Comments
suppressWarnings(rm(VideoComments))
VideoComments <- f_clean_posts(Youtube_all_comments$textDisplay)
print(paste0(length(VideoComments),"- Youtube Comments successfully cleaned"))
# "11523- Youtube Comments successfully cleaned"

# Youtube  : Adding cleaned Youtube Comments
Youtube_all_comments<-cbind(Youtube_all_comments,VideoComments)
rm(VideoComments)
Youtube_all_comments$VideoComments <- as.character(Youtube_all_comments$VideoComments)

# Youtube  : Removing empty Youtube Comments
Youtube_all_comments<- subset(Youtube_all_comments,Youtube_all_comments$VideoComments != "")
print(paste0(length(Youtube_all_comments$VideoComments),"- Valid Youtube comments post removing empty Youtube Posts"))
## [1] "11256- Valid Youtube comments post removing empty Youtube Posts"

# Youtube  : Removing duplicate Youtube Comments and loading into a new table
suppressWarnings(rm(Youtube_comments))
Youtube_comments<-Youtube_all_comments[!duplicated(Youtube_all_comments$VideoComments),]
print(paste0(length(Youtube_comments$VideoComments),"- Valid Youtube posts post removing duplicate Youtube Posts"))
## [1] "9492- Valid Youtube posts post removing duplicate Youtube Posts"

# Youtube  : Removing NAs from Youtube Comments
Youtube_comments <- Youtube_comments[!is.na(Youtube_comments$VideoComments),]
print(paste0(length(Youtube_comments$VideoComments),"- Valid Youtube posts post removing NA from Youtube Posts"))
# [1] "9492- Valid Youtube posts post removing NA from Youtube Posts"

# Youtube  : Structure of data frame
str(Youtube_comments)
summary(Youtube_comments)

# Youtube  : Remove irrelevant columns and renaming column names
colnames(Youtube_comments[c(2:4,6:9,12:15)])
Youtube_comments = subset(Youtube_comments, select = -c(2:4,6:9,12:15) )
# rename column names
colnames(Youtube_comments) <- c("Youtube_videoauthor","Youtube_videoid", "Youtube_commentlikeCount","Youtube_videopublishedAt","Youtube_comments")

# Youtube  : Converting Published Time into valid date format
Youtube_comments$Youtube_videopublishedAt<-gsub("T"," ",Youtube_comments$Youtube_videopublishedAt)
Youtube_comments$Youtube_videopublishedAt<-gsub("+.0000","",Youtube_comments$Youtube_videopublishedAt)
Youtube_comments$Youtube_videopublishedAt<-as.Date(ymd_hms(Youtube_comments$Youtube_videopublishedAt,tz=Sys.timezone()))

# Youtube  : Adding Year, Month and Quarter from Youtube comments
Youtube_comments$Year <- as.numeric(year(Youtube_comments$Youtube_videopublishedAt))
Youtube_comments$Month <- as.numeric(month(Youtube_comments$Youtube_videopublishedAt))
Youtube_comments$Quarter <- as.numeric(quarter(Youtube_comments$Youtube_videopublishedAt))

# Youtube  : Structure of data frame
str(Youtube_comments)
str(Youtube_stats)

# Youtube  : Merging Youtube_comments and Youtube_stats
suppressWarnings(rm(Youtube_data))

colnames(Youtube_stats) <- c("Youtube_videoid","Youtube_viewcount", "Youtube_likecount","Youtube_dislikecount","Youtube_favoritecount","Youtube_commentcount")
Youtube_data<-merge(Youtube_stats, Youtube_comments)
Youtube_data <- Youtube_data %>% dplyr::select(Year,Month,Quarter,everything())
Youtube_data<-Youtube_data[order(Youtube_data$Year,Youtube_data$Month,Youtube_data$Quarter),]

colnames(Youtube_data)


# Youtube  : Loading into a CSV File
YtDataFile <- paste0(Outputdirectory,"Youtube_data.csv")
write.csv(Youtube_data, YtDataFile, row.names = F)
rm(Youtube_stats)
rm(Youtube_comments)
rm(Findstats)
rm(i)

##################################################
# Sentiment Analysis  : Youtube Data
##################################################

# Youtube  : Perform Sentiment Analysis based on the polarity of the individual words
# Scan the words into R
suppressWarnings(rm(hu.liu.pos))
suppressWarnings(rm(hu.liu.neg))
filepos <- paste0(Inputdirectory,"pos.txt")
fileneg <- paste0(Inputdirectory,"neg.txt")
hu.liu.pos = readLines(filepos);
hu.liu.neg = readLines(fileneg);

# If you want to add your words into the positve and negative words list
hu.liu.pos= c(hu.liu.pos,'new','nice','good', 'horizon','tangles','naughty')
hu.liu.neg = c(hu.liu.neg,'down','decreasing','decrease','lower','plastic','wtf', 'behind','feels', 'ugly', 'back','worse','shitty','bad','no','freaking','sucks','horrible')

# Sentiment Function
# The main working principle of sentiment analysis is to find the words that represent positive sentiments and find the words that represent negative sentiments.
# Each word will be given a score of +1 if classified as positive, -1 if negative, and 0 if classified as neutral.
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    # clean up sentences with regex-driven global substitute, gsub():
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by 
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}
# Apply sentiment Function
suppressWarnings(rm(result))
result=suppressWarnings(score.sentiment(Youtube_data$Youtube_comments,hu.liu.pos,hu.liu.neg))

rm(hu.liu.pos)
rm(hu.liu.neg)

# Mapping Scores
result$score[result$score>0] <- "Positive"
result$score[result$score<0] <- "Negative"
result$score[result$score==0] <- "Neutral"

table(result$score)
# Negative  Neutral  Positive 
# 669       4556     4276 

prop.table(table(result$score))
#   Negative   Neutral     Positive 
# 0.06953224   0.47998314  0.45048462


suppressWarnings(rm(Youtube_WordPolarity))
Youtube_WordPolarity <- result$score
Youtube_data <-cbind(Youtube_data,Youtube_WordPolarity)
Youtube_data$Youtube_WordPolarity<-factor(Youtube_data$Youtube_WordPolarity, levels= names(sort(summary(Youtube_data$Youtube_WordPolarity),decreasing = TRUE)))
rm(Youtube_WordPolarity)
rm(result)
print(paste0(sum(count(Youtube_data$Youtube_WordPolarity)$freq)," - Polarities are identified on individual words from the youtube video comments"))
# [1] "9492 - Polarities are identified on individual words from the youtube video comments"

# Youtube  : Structure of data frame
str(Youtube_data)
dim(Youtube_data)

# Youtube  : Loading into a CSV File
write.csv(Youtube_data,YtDataFile, row.names = F)

# Youtube  : Perform Sentiment Analysis based on each word or sentence
# What is get_sentiment function ?
# a) This function which will assess the sentiment of each word or sentence. 
# b) This function takes two arguments: a character vector (of sentences or words) and a “method.”
# c) The method you select determines which of the four available sentiment extraction methods to employ.
# d) We are using syuzhet, bing, afinn and nrc methods to get sentiments.
# e) This function will iterates over a vector of strings and returns sentiment values based on user supplied method.
# f) Return value is a numeric vector of sentiment values, one value for each input sentence.


# What is default method, "syuzhet" ?
# a) It is a custom sentiment dictionary developed in the Nebraska Literary Lab.
# b) The default dictionary should be better tuned to fiction as the terms were extracted from a collection of 165,000 human coded sentences taken from a small corpus of contemporary novels. 
# c) At the time of this release, Syuzhet will only work with languages that use Latin character sets. This effectively means that "Arabic", "Bengali", "Chinese_simplified", "Chinese_traditional", "Greek", "Gujarati", "Hebrew", "Hindi", "Japanese", "Marathi", "Persian", "Russian", "Tamil", "Telugu", "Thai", "Ukranian", "Urdu", "Yiddish" are not supported even though these languages are part of the extended NRC dictionary.

# Get Sentiment Values for a String
suppressWarnings(rm(syuzhet_method))
suppressWarnings(rm(bing_method))
suppressWarnings(rm(afinn_method))
suppressWarnings(rm(nrc_method))

syuzhet_method <- get_sentiment(Youtube_data$Youtube_comments, method="syuzhet")
bing_method  <- get_sentiment(Youtube_data$Youtube_comments, method="bing")
afinn_method  <- get_sentiment(Youtube_data$Youtube_comments, method="afinn")
nrc_method  <- get_sentiment(Youtube_data$Youtube_comments, method="nrc")

summary(syuzhet_method)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.5000  0.0000  0.5000  0.5178  0.8000  7.1500 
summary(bing_method)
#    Min. 1st Qu.  Median    Mean  3rd Qu. Max. 
#  -6.000   0.000   0.000   0.527   1.000   8.000 
summary(afinn_method)
#Min.      1st Qu.  Median  Mean    3rd Qu. Max. 
#-14.000   0.000   2.000   1.782   3.000  25.000 
summary(nrc_method)
# Min.    1st Qu. Median  Mean    3rd Qu.  Max. 
#-4.0000  0.0000  0.0000  0.2422  0.0000 10.0000 


# Different methods will return slightly different results and part of the reason for this is that each method uses a slightly different scale.
# The sign function converts all positive number to 1, all negative numbers to -1 and all zeros remain 0.
# suppressWarnings(rm(yt_syuzhet))
# suppressWarnings(rm(yt_bing))
# suppressWarnings(rm(yt_afinn))
# suppressWarnings(rm(yt_nrc))
# 
# yt_syuzhet <- sign(syuzhet_method)
# yt_bing <- sign(bing_method)
# yt_afinn <- sign(afinn_method)
# yt_nrc <- sign(nrc_method)

suppressWarnings(rm(yt_sentiments))
yt_sentiments <- data.frame(yt_syuzhetScore = syuzhet_method, yt_bingScore = bing_method, 
                            yt_afinnScore = afinn_method, yt_nrcScore = nrc_method)

# Adding Sentiments
Youtube_data <-cbind(Youtube_data,yt_sentiments)
yt_AvgSentiScore <- rowMeans(yt_sentiments)
Youtube_data <-cbind(Youtube_data, yt_AvgSentiScore= yt_AvgSentiScore)
yt_TotSentiScore <- rowSums(yt_sentiments)
Youtube_data <-cbind(Youtube_data, yt_TotSentiScore= yt_TotSentiScore)


# rm(yt_afinn)
# rm(yt_bing)
# rm(yt_nrc)
# rm(yt_syuzhet)
rm(afinn_method)
rm(bing_method)
rm(nrc_method)
rm(syuzhet_method)
rm(yt_sentiments)
rm(yt_AvgSentiScore)
rm(yt_TotSentiScore)

# Youtube  : Loading into a CSV File
write.csv(Youtube_data, YtDataFile, row.names = F)

# Youtube  : Perform Sentiment Analysis based on the polarity and emotions on individual words or sentence.
# What is get_nrc_sentiment function ?
# a) The get_nrc_sentiment implements Saif Mohammad's - NRC Word-Emotion Association Lexicon aka EmoLex.
# b) According to Mohammad, the NRC emotion lexicon is a list of words and their associations with eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive)”
# c)  The get_nrc_sentiment function returns a data frame in which each row represents a sentence from the original file
# d) The columns include one for each emotion type was well as the positive or negative sentiment valence.
# d) Citation: http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

# Get Emotions and Valence from NRC Dictionary
suppressWarnings(rm(yt_emotions))
yt_emotions <- get_nrc_sentiment(Youtube_data$Youtube_comments)

# Custom Words for each emotion
suppressWarnings(rm(fear_emotion))
suppressWarnings(rm(trust_emotion))
fear_emotion <- 'plastic'   # Keep Adding words which creates emotion fear example 'plastic|birds'
trust_emotion <- 'hope'

# finding words with fear emotion
suppressWarnings(rm(fear_pattern_pos))
fear_pattern_pos<-grep(pattern = fear_emotion, x = Youtube_data$Youtube_video_comments , value = F)
fear_pattern_pos<-data.frame(fear_pattern_pos)

# finding words with trust emotion
suppressWarnings(rm(trust_pattern_pos))
trust_pattern_pos<-grep(pattern = trust_emotion, x = Youtube_data$Youtube_video_comments , value = F)
trust_pattern_pos<-data.frame(trust_pattern_pos)

# Calling function to update fear
suppressWarnings(rm(testfear))
suppressWarnings(rm(fear_updated))
testfear <- function(x) { yt_emotions$fear[x]<<-1}
fear_updated<-apply(fear_pattern_pos,2,function(x) testfear(x))

# Calling function to update trust
suppressWarnings(rm(testtrust))
suppressWarnings(rm(trust_updated))
testtrust <- function(x) { yt_emotions$trust[x]<<-1}
trust_updated<-apply(trust_pattern_pos,2,function(x) testtrust(x))

# Creating Percentages and sorting based on emotions
suppressWarnings(rm(emo_bar))
suppressWarnings(rm(yt_emo_per))

emo_bar = sort(colSums(prop.table(yt_emotions)))
yt_emo_per = data.frame(Percentage=emo_bar, Emotion=names(emo_bar))
yt_emo_per$Emotion = factor(yt_emo_per$Emotion, levels=yt_emo_per$Emotion[order(yt_emo_per$Percentage, decreasing = TRUE)])

colnames(yt_emotions) <- c("Youtube_anger", "Youtube_anticipation","Youtube_disgust",
                           "Youtube_fear", "Youtube_joy", "Youtube_sadness", "Youtube_surprise",
                           "Youtube_trust","Youtube_negative","Youtube_positive")

# Adding Emotions 
Youtube_data <-cbind(Youtube_data,yt_emotions)
print("Eight basic emotions - anger, fear, anticipation, trust, surprise, sadness, joy, and disgust and two sentiments - negative and positive are identified")

rm(fear_emotion)
rm(trust_emotion)
rm(fear_updated)
rm(trust_updated)
rm(fear_pattern_pos)
rm(trust_pattern_pos)

# Youtube  : Structure of data frame
class(Youtube_data)
str(Youtube_data)
dim(Youtube_data)

# Youtube  : Loading into a CSV File
write.csv(Youtube_data,YtDataFile, row.names = F)

# Youtube  : Perform Sentiment Analysis based on the Comparison Word Cloud using emotions
# put everything in a single vector
suppressWarnings(rm(yt_all))
yt_all = c(
  paste(Youtube_data$Youtube_comments[Youtube_data$Youtube_anger>0], collapse=" "),
  paste(Youtube_data$Youtube_comments[Youtube_data$Youtube_anticipation > 0], collapse=" "),
  paste(Youtube_data$Youtube_comments[Youtube_data$Youtube_disgust > 0], collapse=" "),
  paste(Youtube_data$Youtube_comments[Youtube_data$Youtube_fear > 0], collapse=" "),
  paste(Youtube_data$Youtube_comments[Youtube_data$Youtube_joy > 0], collapse=" "),
  paste(Youtube_data$Youtube_comments[Youtube_data$Youtube_sadness > 0], collapse=" "),
  paste(Youtube_data$Youtube_comments[Youtube_data$Youtube_surprise > 0], collapse=" "),
  paste(Youtube_data$Youtube_comments[Youtube_data$Youtube_trust > 0], collapse=" ")
)

# Remove common words like the, is, at, which, and on.
yt_all <- removeWords(yt_all, stopwords("english"))

# Corpus and term-document matrix

# create corpus
# Corpus is a collection of text documents and VectorSource is for only character vectors
# Corpus will not allow you to keep dashes, underscores or other signs of punctuation and it automatically removes them.
suppressWarnings(rm(yt_corpus))
yt_corpus = Corpus(VectorSource(yt_all))

# create term-document matrix
# A document-term matrix or term-document matrix is a mathematical matrix that describes the frequency of terms that occur in a collection of documents
# In a document-term matrix, rows correspond to documents in the collection and columns correspond to terms.
suppressWarnings(rm(yt_tdm))
yt_tdm = TermDocumentMatrix(yt_corpus)

# convert as matrix
suppressWarnings(rm(yt_tdm1))
yt_tdm = as.matrix(yt_tdm)
yt_tdm1 <- yt_tdm[nchar(rownames(yt_tdm)) < 11,]

# add column names
colnames(yt_tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(yt_tdm1) <- colnames(yt_tdm)

cat("\n")
print(" Completed - Sentiment Analysis based on the Comparison Word Cloud using emotions")

##################################################
# Plotting  : Sentiments based on Youtube Data
##################################################

# Youtube  : Plot Sentiments based on the words of each comments

# Below is data processing for plotting sentiments
YT_plotdata <- Youtube_data[,c( 1:3, 14:18, 21:28)]
colnames(YT_plotdata)

colnames(YT_plotdata) <- c("Year","Month","Quarter","Polarity","SyuzhetScore","BingScore" ,
                           "AfinnScore","NRCscore","Anger","Anticipation","Disgust","Fear",
                           "Joy","Sadness","Surprise","Trust")

#YT_plotdata <- YT_plotdata %>% filter( Date >= '2014-10-01' & Date <= '2017-09-30')
YT_plotdata <- YT_plotdata %>% mutate( YearQuarter = paste0(substr(as.character(Year),3,4),'Q',Quarter))

# check in table and proportion table

table(YT_plotdata$YearQuarter, YT_plotdata$Polarity)
#         Neutral Positive Negative
# 15Q2      10        1        2
# 15Q3       0        4        0
# 15Q4      42        9        4
# 16Q1       1        2        0
# 16Q2      86       94       24
# 16Q3      79       74       57
# 16Q4      83       92       18
# 17Q1     313      416       62
# 17Q2    1182      886      136
# 17Q3     546      302       82
# 17Q4    1193     1383      135
# 18Q1    1021     1013      140

prop.table(table(YT_plotdata$YearQuarter, YT_plotdata$Polarity),1)
#         Neutral   Positive   Negative
# 15Q2 0.76923077 0.07692308 0.15384615
# 15Q3 0.00000000 1.00000000 0.00000000
# 15Q4 0.76363636 0.16363636 0.07272727
# 16Q1 0.33333333 0.66666667 0.00000000
# 16Q2 0.42156863 0.46078431 0.11764706
# 16Q3 0.37619048 0.35238095 0.27142857
# 16Q4 0.43005181 0.47668394 0.09326425
# 17Q1 0.39570164 0.52591656 0.07838180
# 17Q2 0.53629764 0.40199637 0.06170599
# 17Q3 0.58709677 0.32473118 0.08817204
# 17Q4 0.44005902 0.51014386 0.04979712
# 18Q1 0.46964121 0.46596136 0.06439742

# Ploarity plot quarter on quarter
ggplot(data = YT_plotdata, aes(x = YearQuarter, fill = Polarity )) + 
  geom_bar() + theme(panel.background = element_blank(),
                     axis.line.y = element_line(colour = "black",linetype = "solid"),
                     axis.line.x = element_line(colour = "black",linetype = "solid")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12),
    axis.title.y = element_text(color="black",face="bold", size=14),
    axis.text = element_text(color="black",size = 8),
    title = element_text(color="black",face="bold", size=14)
  )+theme(
    legend.position = c(0.1,0.2),
    legend.title = element_text(face="bold",size=10), 
    legend.text = element_text(color="black",size = 8))+
  scale_fill_manual(values = c("skyBlue","lightGreen","#e41a1c"))+
  labs(title="Kurkure Polarity trend based on YouTube interactions",x="Year and Quarter",y="word count")


# Emotion plot : quarter on quarter
library(reshape2)
emotion_data <- melt(YT_plotdata[,c(9:17)] %>% group_by(YearQuarter) %>% summarise_all(sum))

colnames(emotion_data)[2] <- "Emotion"

ggplot(emotion_data,aes( x= YearQuarter, y=value , fill = Emotion)) + 
  geom_bar( stat = 'identity',position = "fill") + 
  theme(panel.background = element_blank()) + theme(
    axis.title.x = element_text(color="black",face="bold", size=12),
    axis.title.y = element_text(color="black",face="bold", size=14),
    axis.text = element_text(color="black",size = 8),
    title = element_text(color="black",face="bold", size=14)
  )+
  theme(
    legend.title = element_text(face="bold",size=10), 
    legend.text = element_text(color="black",size = 8))+
  scale_fill_manual(values = c('#e41a1c',"#b2df8a","#a65628","#4d4d4d","#fee08b","#e0e0e0","#f46d43","#3288bd"))+
  labs(title="Kurkure Emotions trend for YouTube Interactions ",x="Year and Quarter",y="Percentage")


#-----------------------------
# Sentiment score plots
#
# YouTube : Plot the different sentiments from different methods
#-----------------------------------
YT_plotdata <- YT_plotdata %>% mutate( YearMonth = paste0(substr(as.character(Year),3,4),'/M',Month))

YT_plotdata <- YT_plotdata %>% arrange(Year,Month)

YT_SentiScore_plotdata <- YT_plotdata[,c(1,2,5:8,18)] %>% 
  group_by(YearMonth) %>% 
  summarise(SyuzhetScore = sum(SyuzhetScore),
            BingScore = sum(BingScore),
            AfinnScore = sum(AfinnScore),
            NRCscore = sum(NRCscore),
            Year = mean(Year),
            Month = mean(Month)) 

str(YT_SentiScore_plotdata)
YT_SentiScore_plotdata$BingScore <- as.numeric(YT_SentiScore_plotdata$BingScore)
YT_SentiScore_plotdata$AfinnScore <- as.numeric(YT_SentiScore_plotdata$AfinnScore)

YT_SentiScore_plotdata <- YT_SentiScore_plotdata %>% arrange(Year,Month) # order date by Year and Month for plotting

# read sales data
salesFile <- paste0(Inputdirectory,"Kurkure_SalesVolumeData.csv")

sales <- read.csv(salesFile, header = TRUE, sep= '\t' ,stringsAsFactors = FALSE)
colnames(sales)[1]<- "Date"
str(sales)
sales$DelhiandNCR<-round(as.numeric(gsub(",","",sales$DelhiandNCR)))
sales$Kolkata<-round(as.numeric(gsub(",","",sales$Kolkata)))
sales$Mumbai<-round(as.numeric(gsub(",","",sales$Mumbai)))
sales$Hyderabad<-round(as.numeric(gsub(",","",sales$Hyderabad)))
sales$Chennai<-round(as.numeric(gsub(",","",sales$Chennai)))
sales$TotalSalesVolume <- round(rowSums(sales[,c(2:6)]))

sales$Date <- format(as.Date(paste("01",sub(pattern = "-", replacement = "-20",  x = sales$Date), sep="-"),"%d-%b-%Y"),"%Y-%m-%d")
sales$Date<-as.Date(sales$Date)
sales <- sales %>% mutate(Year = year(Date),Month = month(Date))
sales <- sales %>% mutate( YearMonth = paste0(substr(as.character(Year),3,4),'/M',Month))


YT_SentiScore_plotdata <- merge(YT_SentiScore_plotdata,sales)

YT_SentiScore_plotdata <- YT_SentiScore_plotdata %>% arrange(Year, Month)

YT_SentiScore_plotdata <- YT_SentiScore_plotdata[,c(1,4:7,14)]

dat <- YT_SentiScore_plotdata
dat$YearMonth <- as.factor(dat$YearMonth)

dat[,2:6] <- data.frame(scale(dat[,-1]))
dat <- reshape2::melt(dat,factorsAsStrings=F)

n = nrow(YT_SentiScore_plotdata)
n
x_lables = as.character(dat[seq(1, n,1),1])
breaks = seq(1,n,1)

x_lables

dat$YearMonth

ggplot(data=dat,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="Year and Month",y="")+
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.y = element_line(colour = "black",linetype = "solid"),
        axis.line.x = element_line(colour = "black",linetype = "solid"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12),
    axis.text.x =  element_text(color="black", face= "bold.italic",size = 10, angle = 60),
    axis.text.y =  element_text(color="black",size = 10 ),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete("Year and Month", labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.5,0.8),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('#e41a1c',"#377eb8","#ff7f00","darkgreen","#f781bf"))


# let us plot Syuzhet Score and Sales Volume
dat_Syuzhet <- data.frame(scale(YT_SentiScore_plotdata[,c(-1,-3:-5)]))
dat_Syuzhet <- cbind(YT_SentiScore_plotdata$YearMonth,dat_Syuzhet)
dat_Syuzhet <- melt(dat_Syuzhet)
colnames(dat_Syuzhet)[1] <- "YearMonth"

ggplot(data=dat_Syuzhet,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="Year and Month",y="")+
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.y = element_line(colour = "black",linetype = "solid"),
        axis.line.x = element_line(colour = "black",linetype = "solid"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12),
    axis.text.x =  element_text(color="black", face= "bold.italic",size = 10, angle = 60),
    axis.text.y =  element_text(color="black",size = 10 ),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete("Year and Month", labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2,0.9),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('blue',"green"))


# let us plot BingScore and Sales Volume

dat_Bing <- data.frame(scale(YT_SentiScore_plotdata[,c(-1,-2, -4,-5)]))
dat_Bing <- cbind(YT_SentiScore_plotdata$YearMonth,dat_Bing)
dat_Bing <- melt(dat_Bing)
colnames(dat_Bing)[1] <- "YearMonth"

ggplot(data=dat_Bing,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="",y="")+
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.y = element_line(colour = "black",linetype = "solid"),
        axis.line.x = element_line(colour = "black",linetype = "solid"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12),
    axis.text.x =  element_text(color="black", face= "bold.italic",size = 10, angle = 60),
    axis.text.y =  element_text(color="black",size = 10 ),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete("Year and Month", labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2,0.9),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('blue',"green"))

# let us plot AfinnScore and Sales Volume

dat_Afinn <- data.frame(scale(YT_SentiScore_plotdata[,c(-1:-3,-5)]))
dat_Afinn <- cbind(YT_SentiScore_plotdata$YearMonth,dat_Afinn)
dat_Afinn <- melt(dat_Afinn)
colnames(dat_Afinn)[1] <- "YearMonth"

ggplot(data=dat_Afinn,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="",y="")+
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.y = element_line(colour = "black",linetype = "solid"),
        axis.line.x = element_line(colour = "black",linetype = "solid"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12),
    axis.text.x =  element_text(color="black", face= "bold.italic",size = 10, angle = 60),
    axis.text.y =  element_text(color="black",size = 10 ),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete("Year and Month", labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2,0.9),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('blue',"green"))

# let us plot nrcScore and Sales Volume

dat_nrc <- data.frame(scale(YT_SentiScore_plotdata[,c(-1:-4)]))
dat_nrc <- cbind(YT_SentiScore_plotdata$YearMonth,dat_nrc)
dat_nrc <- melt(dat_nrc)
colnames(dat_nrc)[1] <- "YearMonth"

ggplot(data=dat_nrc,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="",y="")+
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.y = element_line(colour = "black",linetype = "solid"),
        axis.line.x = element_line(colour = "black",linetype = "solid"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12),
    axis.text.x =  element_text(color="black", face= "bold.italic",size = 10, angle = 60),
    axis.text.y =  element_text(color="black",size = 10 ),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete("Year and Month", labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2,0.9),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('blue',"green"))



# Youtube : Plot comparison wordcloud 
comparison.cloud(yt_tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(0.9, 0.4),rot.per=0.4)

# Youtube : Plot commonality cloud
commonality.cloud(yt_tdm1, random.order=FALSE, colors = brewer.pal(8, "Dark2"))





