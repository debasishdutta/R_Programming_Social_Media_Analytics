##################################################
# Social Media  Platform : Facebook 
# Extract data for sentiment analysis
##################################################

# Set Variables required for the analysis
Outputdirectory                 <-"****************"
Inputdirectory                  <- "******************"
Modeloutdirectory               <- "****************"
facebook_searchstring           <-"kurkure"
FacebookPage_talkingaboutcount  <- 25
FacebookPage_fromdate           <-'2013-12-01'
FacebookPage_tilldate           <-'2017-12-30'
error_page <- " - Facebook Page does not exist"
error_comment <- " - Facebook Post Comments does not exist"

fear_emotion <- c('plastic','fake')   # Keep Adding words which creates emotion fear example 'plastic|birds'
trust_emotion <- 'hope'

# Set Working directory
setwd("*****************")
print (paste0("Current Working Directory - ",getwd()))

# Loading all the libraries
suppressWarnings(library(Rfacebook)) # This library will support fbOAuth function
suppressWarnings(library(lubridate)) # This library will support ymd_hms function
suppressWarnings(library(dplyr))
suppressWarnings(library(syuzhet)) # This library will support get_sentiment() function.
suppressWarnings(library(tm)) # This library will support Corpus, removeWords and TermDocumentMatrix functions
suppressWarnings(library(wordcloud)) # This library will support comparison.cloud and commonality.cloud
suppressWarnings(library(MASS)) # this is to run stepAIC
suppressWarnings(library(car)) # this is required to check lm model assumptions
suppressWarnings(library(ggplot2)) # this is for visualization
suppressWarnings(library(psych)) # this is to check correlation among variables

# Facebook  : Sets up the OAuth credentials for a facebook session
# First time use fbOAuth function and save it to local folder
#fb_oauth <- fbOAuth(app_id=facebook_appid, app_secret=facebook_appsecret,extended_permissions = TRUE)
load("fb_oauth")

# Facebook  : Check your profile account information
me <- getUsers("me",token=fb_oauth, private_info=TRUE)
print(paste0(me$name,"- facebook profile loaded successfully"))
rm(me)
# Facebook  : Search all Pages that contain a certain keyword
suppressWarnings(rm(Facebook_all_pages))
Facebook_all_pages<-suppressWarnings(searchPages(string=facebook_searchstring, token=fb_oauth, n=500))

# Facebook  : Structure of data frame
str(Facebook_all_pages)
dim(Facebook_all_pages)

# Facebook  : Loading into a CSV File
filepath <- paste0(Outputdirectory,"Facebook_all_pages.csv")
write.csv(Facebook_all_pages,filepath, row.names = F)

# Facebook  : Extracting only those Pages whose talking_about_count > a number and username != "NA"
# Reference : https://developers.facebook.com/docs/graph-api/reference/page/
# Field     : "talking_about_count" refers to "The number of people talking"
# Field     : "username" refers to The alias of the Page. For example, for www.facebook.com/platform the username is 'platform'
suppressWarnings(rm(Facebook_pages))

Facebook_pages <- Facebook_all_pages %>% 
  filter(talking_about_count > FacebookPage_talkingaboutcount) %>%
  filter(username != "NA")
  
rm(Facebook_all_pages)

# Check talking count of extracted pages
Facebook_pages%>%group_by(Page_Alias=username)%>%summarise(Talking_count_sum=sum(talking_about_count))

# Facebook  : Extract list of facebook posts from all the shortlisted pages for required duration
suppressWarnings(rm(Facebook_posts))
Facebook_posts<-''
for(i in 1:length(Facebook_pages$username))
{
  print (Facebook_pages$username[i])
  Facebook_posts<-rbind(Facebook_posts,tryCatch(getPage(Facebook_pages$username[i], token=fb_oauth, n=5000,since=FacebookPage_fromdate, until=FacebookPage_tilldate, feed = TRUE),error=function(e) print(paste0(Facebook_pages$username[i], error_page)), finally=print(paste0(Facebook_pages$username[i]," - Page Processed"))))
}
print(paste0(nrow(Facebook_posts)," - facebook posts successfully extracted from valid facebook pages"))
# [1] "2695 - facebook posts successfully extracted from valid facebook pages"
# Facebook  : Structure of data frame
str(Facebook_posts)

# Facebook  : Loading into a CSV File all extracted posts which is about 1779
# 1571 from 1st , 1058 from 2nd and 65 from 3rd one
filepath <- paste0(Outputdirectory,"Facebook_all_posts.csv")
write.csv(Facebook_posts,filepath, row.names = F)

rm(error_page)
rm(i)
rm(Facebook_pages)


# Facebook  : Text Data Cleaning Function
f_clean_posts <- function (fbposts) 
{
  # remove re entities
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
  #trim white spaces
  clean_fbposts = trimws(clean_fbposts)
  clean_fbposts
}

# Facebook  : Converting Created Time into valid date format
tmdate<- as.Date(ymd_hms(Facebook_posts$created_time))
Facebook_posts$created_time <- tmdate
rm(tmdate)

# Facebook  : Adding Year, Month and Quarter from facebook Post Created Time
Facebook_posts <- Facebook_posts %>% mutate(Year = year(created_time),
                Month = month(created_time),
                Quarter = quarter(created_time))

# Facebook  : Displaying total count of facebook pages and posts
print(paste0(length(unique(Facebook_posts$from_name)),"- Total unique facebook posts users"))
print(paste0(length(Facebook_posts$message),"- Total facebook posts"))

# first row is blank remvoe it
Facebook_posts <- Facebook_posts[-1,]

# Facebook  : Extracting Facebook Comments for each of the valid Facebook Post
suppressWarnings(rm(FindPostsComments))
FindPostsComments<-''
print (paste0("Total Facebook Posts waiting to process - ",length(Facebook_posts$id)))
print (" #### NEXT RUN WILL TAKE TIME - GO TAKE A BREAK #### ")
for(i in 1:length(Facebook_posts$id))
{
  if (Facebook_posts$comments_count[i] > 0)
  {
    cat("post number= ",i,"of",length(Facebook_posts$id) , "post id = ", Facebook_posts$id[i], "post comments count = ", Facebook_posts$comments_count[i])
    FindPostsComments<-rbind(FindPostsComments,tryCatch(getPost(Facebook_posts$id[i], token=fb_oauth, n=50000,comments = TRUE),error=function(e) print(paste0(Facebook_posts$id[i], error_comment)), finally=print(paste0(Facebook_posts$id[i]," - Processed"))))
    
  }
}
suppressWarnings(rm(Facebook_comments))
Facebook_comments <- data.frame()
for(i in 1:nrow(FindPostsComments))
{
    print (paste0("First Level Passed-",i))
      if (length(FindPostsComments[i,]$comments)!=1)
      {
        print (paste0("Second Level Passed-",length(FindPostsComments[i,]$comments)))
        if (length(FindPostsComments[i,]$comments$message)!=0)
        {
          print (paste0("Last Level Passed -",length(FindPostsComments[i,]$comments$message)))
          Facebook_comments<-rbind(Facebook_comments,data.frame(FindPostsComments[i,]$post,FindPostsComments[i,]$comments))
        }
      }
  cat("\n")
  print (paste0("In Progress - Total Comments Loaded Successfully till now - ",nrow(Facebook_comments)))
} 
print (paste0("Completed - Total Comments Loaded Successfully - ",nrow(Facebook_comments)))
# [1] "Completed - Total Comments Loaded Successfully - 139217"

rm(FindPostsComments)
rm(i)
rm(error_comment)
# Facebook  : Structure of data frame
str(Facebook_comments)
colnames(Facebook_comments)
# Facebook  : Remove columns not required for the analysis/plotting and renaming column names
# columns that will be removed are "from_name", "from_id.1" , "from_name.1
colnames(Facebook_comments[c(11,12)])
Facebook_comments = subset(Facebook_comments, select = -c(11,12) )
colnames(Facebook_comments) <- c("Facebook_Page_ID","Facebook_Page_Name","Facebook_Post",
                                 "Facebook_Post_CreatedTime", "Facebook_Post_Type","Facebook_Post_Link",
                                 "Facebook_Post_ID","Facebook_Post_LikesCount","Facebook_Post_CommentsCount",
                                 "Facebook_Post_SharesCount","Facebook_Post_Comments",
                                 "Facebook_Post_Comments_CreatedTime","Facebook_Post_Comments_LikesCount",
                                 "Facebook_Post_Comments_CommentsCount","Facebook_Post_Comments_ID")

# Facebook  : Structure of data frame
str(Facebook_comments)

# Facebook  : Calling function to clean the facebook comments
suppressWarnings(rm(fb_posts_comments))
fb_posts_comments <- f_clean_posts(Facebook_comments$Facebook_Post_Comments)
print(paste0(length(fb_posts_comments)," - facebook comments successfully cleaned"))

# Facebook  : Adding cleaned facebook comments
Facebook_comments<-cbind(Facebook_comments,fb_posts_comments)
Facebook_comments$fb_posts_comments <- as.character(Facebook_comments$fb_posts_comments)
rm(fb_posts_comments)
# Facebook  : Removing empty facebook comments
Facebook_comments<- subset(Facebook_comments,Facebook_comments$fb_posts_comments != "")
print(paste0(length(Facebook_comments$fb_posts_comments),"- Valid facebook posts post removing empty facebook comments"))
# [1] "57302- Valid facebook posts post removing empty facebook comments"

# Facebook  : Removing duplicate facebook comments and loading into a new table
suppressWarnings(rm(Facebook_data))
Facebook_data<-Facebook_comments[!duplicated(Facebook_comments$fb_posts_comments),]
print(paste0(length(Facebook_data$fb_posts_comments),"- Valid facebook comments after removing duplicate facebook comments"))
# [1] "23814- Valid facebook comments after removing duplicate facebook comments"

# Facebook  : Removing NAs from facebook comments
Facebook_data <- Facebook_data[!is.na(Facebook_data$fb_posts_comments),]
print(paste0(length(Facebook_data$fb_posts_comments),"- Valid facebook comments after removing NA from facebook comments"))
# [1] "23814- Valid facebook comments after removing NA from facebook comments"


# Facebook  : Structure of data frame
str(Facebook_data)
class(Facebook_data$Facebook_Post_CreatedTime)
class(Facebook_data$Facebook_Post_Comments_CreatedTime)

# Facebook  : Converting Created Time into valid date format
Facebook_data$Facebook_Post_CreatedTime<-as.Date(ymd_hms(Facebook_data$Facebook_Post_CreatedTime,tz=Sys.timezone()))
Facebook_data$Facebook_Post_Comments_CreatedTime<-as.Date(ymd_hms(Facebook_data$Facebook_Post_Comments_CreatedTime,tz=Sys.timezone()))


# Facebook  : Adding Year, Month and Quarter from facebook Post Created Time
Facebook_data <- Facebook_data %>% mutate(Facebook_Post_Comments_CreatedYear = year(Facebook_Post_Comments_CreatedTime),
                                          Facebook_Post_Comments_CreatedMonth = month(Facebook_Post_Comments_CreatedTime),
                                          Facebook_Post_Comments_CreatedQuarter = quarter(Facebook_Post_Comments_CreatedTime))

# Facebook  : Structure of data frame
dim(Facebook_data)
str(Facebook_data)

# Facebook  : Remove irrelevant columns and renaming column names
colnames(Facebook_data[11])
Facebook_data <- Facebook_data[,-11]
colnames(Facebook_data[15])
colnames(Facebook_data)[15]<-"Facebook_Post_Comments"
colnames(Facebook_data[15])

# Facebook  : Loading into a CSV File
filepath <- paste0(Outputdirectory,"Facebook_data.csv")
write.csv(Facebook_data,filepath, row.names = F)
rm(Facebook_comments)

##################################################
# Sentiment Analysis  : Facebook Data
##################################################

# Facebook  : Perform Sentiment Analysis based on the polarity of the individual words
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
# The main working principle of sentiment analysis is to find the words that represent positive sentiments
# and negative sentiments.
# Each word will be given a score of +1 if classified as positive, -1 if negative, and 0 if neutral.
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr);
  require(stringr);
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
result=suppressWarnings(score.sentiment(Facebook_data$Facebook_Post_Comments,hu.liu.pos,hu.liu.neg))
# Mapping Scores
result$score[result$score>0] <- "Positive"
result$score[result$score<0] <- "Negative"
result$score[result$score==0] <- "Neutral"

table(result$score)
# Negative  Neutral   Positive 
# 1585      16570     5659 

prop.table(table(result$score))
#   Negative    Neutral     Positive 
# 0.06655749    0.69580919  0.23763332

suppressWarnings(rm(Word_Polarity))
Facebook_Word_Polarity <- result$score
Facebook_data <-cbind(Facebook_data,Facebook_Word_Polarity)
Facebook_data$Facebook_Word_Polarity<-factor(Facebook_data$Facebook_Word_Polarity, levels= names(sort(summary(Facebook_data$Facebook_Word_Polarity),decreasing = TRUE)))

print(paste0(sum(count(Facebook_data$Facebook_Word_Polarity)$freq)," - Polarities are identified on individual words from the facebook post"))
# [1] "23814 - Polarities are identified on individual words from the facebook post"

# Facebook  : Structure of data frame
dim(Facebook_data)
# [1] 23814    19

# Facebook  : Loading into a CSV File
filepath <- paste0(Outputdirectory,"Facebook_data.csv")
write.csv(Facebook_data,filepath, row.names = F)

# Facebook  : Perform Sentiment Analysis based on each word or sentence
# What is get_sentiment function ?
# a) This function which will assess the sentiment of each word or sentence. 
# b) This function takes two arguments: a character vector (of sentences or words) and a method
# c) The method you select determines which of the four available sentiment extraction methods to employ.
# d) We are using syuzhet, bing, afinn and nrc methods to get sentiments.
# e) This function will iterates over a vector of strings and returns sentiment values based on user supplied method.
# f) Return value is a numeric vector of sentiment values, one value for each input sentence.


# What is default method, "syuzhet" ?
# a) It is a custom sentiment dictionary developed in the Nebraska Literary Lab.
# b) The default dictionary should be better tuned to fiction as the terms were extracted from a collection of 165,000 human coded sentences taken from a small corpus of contemporary novels. 
# c) At the time of this release, Syuzhet will only work with languages that use Latin character sets. 
#This effectively means that "Arabic", "Bengali", "Chinese_simplified", "Chinese_traditional", "Greek", "Gujarati", "Hebrew",
#"Hindi", "Japanese", "Marathi", "Persian", "Russian", "Tamil", "Telugu", "Thai", "Ukranian", "Urdu", "Yiddish" are not supported even though these languages are part of the extended NRC dictionary.

# Get Sentiment Values for a String
suppressWarnings(rm(syuzhet_method))
suppressWarnings(rm(bing_method))
suppressWarnings(rm(afinn_method))
suppressWarnings(rm(nrc_method))

syuzhet_method <- get_sentiment(Facebook_data$Facebook_Post_Comments, method="syuzhet")
bing_method  <- get_sentiment(Facebook_data$Facebook_Post_Comments, method="bing")
afinn_method  <- get_sentiment(Facebook_data$Facebook_Post_Comments, method="afinn")
nrc_method  <- get_sentiment(Facebook_data$Facebook_Post_Comments, method="nrc")

summary(syuzhet_method)
#    Min. 1st Qu.  Median   Mean    3rd Qu.  Max. 
#-2.5000  0.0000  0.0000    0.3378  0.6000   28.2500 
summary(bing_method)
#    Min. 1st Qu.  Median    Mean  3rd Qu. Max. 
# -7.0000  0.0000  0.0000  0.267   0.0000  28.0000
summary(afinn_method)
#Min.      1st Qu.  Median  Mean    3rd Qu. Max. 
#-13.000   0.000    0.000   0.8438   1.000   56.000 
summary(nrc_method)
# Min.    1st Qu. Median  Mean    3rd Qu.  Max. 
#-4.0000  0.0000  0.0000  0.2844  0.0000   40.0000 

# Different methods will return slightly different results and part of the reason for this is that each method uses a slightly different scale.
# The sign function converts all positive number to 1, all negative numbers to -1 and all zeros remain 0.
# suppressWarnings(rm(fb_syuzhet))
# suppressWarnings(rm(fb_bing))
# suppressWarnings(rm(fb_afinn))
# suppressWarnings(rm(fb_nrc))
# 
# fb_syuzhet <- sign(syuzhet_method)
# fb_bing <- sign(bing_method)
# fb_afinn <- sign(afinn_method)
# fb_nrc <- sign(nrc_method)

suppressWarnings(rm(fb_sentiments))
fb_sentiments <- data.frame(Facebook_data$Facebook_Post_Comments,fb_syuzhetScore = syuzhet_method,
                            fb_bingScore = bing_method, fb_afinnScore = afinn_method,
                            fb_nrcScore = nrc_method)

# Adding Sentiments
Facebook_data <-cbind(Facebook_data,fb_sentiments[,2:5])
fb_meanscore <- rowMeans(fb_sentiments[,2:5])
Facebook_data <-cbind(Facebook_data,fb_meanscore)

# Facebook  : Loading into a CSV File
write.csv(Facebook_data,filepath, row.names = F)

# Facebook  : Perform Sentiment Analysis based on the polarity and emotions on individual words or sentence.
# What is get_nrc_sentiment function ?
# a) The get_nrc_sentiment implements Saif Mohammad's - NRC Word-Emotion Association Lexicon aka EmoLex.
# b) According to Mohammad, the NRC emotion lexicon is a list of words and their associations with eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive)”
# c)  The get_nrc_sentiment function returns a data frame in which each row represents a sentence from the original file
# d) The columns include one for each emotion type was well as the positive or negative sentiment valence.
# d) Citation: http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

# Get Emotions and Valence from NRC Dictionary
suppressWarnings(rm(fb_emotions))
fb_emotions <- get_nrc_sentiment(Facebook_data$Facebook_Post_Comments)



# finding words with fear emotion
suppressWarnings(rm(fear_pattern_pos))
fear_pattern_pos<-grep(pattern = fear_emotion, x = Facebook_data$Facebook_Post_Comments , value = F)
fear_pattern_pos<-data.frame(fear_pattern_pos)
  
# finding words with trust emotion
suppressWarnings(rm(trust_pattern_pos))
trust_pattern_pos<-grep(pattern = trust_emotion, x = Facebook_data$Facebook_Post_Comments , value = F)
trust_pattern_pos<-data.frame(trust_pattern_pos)

# Calling function to update fear
suppressWarnings(rm(testfear))
suppressWarnings(rm(fear_updated))
testfear <- function(x) { fb_emotions$fear[x]<<-1}
fear_updated<-apply(fear_pattern_pos,2,function(x) testfear(x))

# Calling function to update trust
suppressWarnings(rm(testtrust))
suppressWarnings(rm(trust_updated))
testtrust <- function(x) { fb_emotions$trust[x]<<-1}
trust_updated<-apply(trust_pattern_pos,2,function(x) testtrust(x))

# Creating Percentages and sorting based on emotions
suppressWarnings(rm(emo_bar))
suppressWarnings(rm(fb_emo_per))

emo_bar = sort(colSums(prop.table(fb_emotions)))
fb_emo_per = data.frame(Percentage=emo_bar, Emotion=names(emo_bar))
fb_emo_per$Emotion = factor(fb_emo_per$Emotion, levels=fb_emo_per$Emotion[order(fb_emo_per$Percentage, decreasing = TRUE)])

# Adding Emotions 
Facebook_data <-cbind(Facebook_data,fb_emotions)
print("Eight basic emotions - anger, fear, anticipation, trust, surprise, sadness, joy, and disgust and two sentiments - negative and positive are identified")

# Facebook  : Loading into a CSV File
write.csv(Facebook_data,filepath, row.names = F)

# Facebook  : Perform Sentiment Analysis based on the Comparison Word Cloud using emotions
# put everything in a single vector
suppressWarnings(rm(fb_all))
fb_all = c(
  paste(Facebook_data$Facebook_Post_Comments[Facebook_data$anger>0], collapse=" "),
  paste(Facebook_data$Facebook_Post_Comments[Facebook_data$anticipation > 0], collapse=" "),
  paste(Facebook_data$Facebook_Post_Comments[Facebook_data$disgust > 0], collapse=" "),
  paste(Facebook_data$Facebook_Post_Comments[Facebook_data$fear > 0], collapse=" "),
  paste(Facebook_data$Facebook_Post_Comments[Facebook_data$joy > 0], collapse=" "),
  paste(Facebook_data$Facebook_Post_Comments[Facebook_data$sadness > 0], collapse=" "),
  paste(Facebook_data$Facebook_Post_Comments[Facebook_data$surprise > 0], collapse=" "),
  paste(Facebook_data$Facebook_Post_Comments[Facebook_data$trust > 0], collapse=" ")
)

# Remove common words like the, is, at, which, and on.
fb_all <- removeWords(fb_all, stopwords("english"))

# Corpus and term-document matrix

# create corpus
# Corpus is a collection of text documents and VectorSource is for only character vectors
# Corpus will not allow you to keep dashes, underscores or other signs of punctuation and it automatically removes them.
suppressWarnings(rm(fb_corpus))
fb_corpus = Corpus(VectorSource(fb_all))

# create term-document matrix
# A document-term matrix or term-document matrix is a mathematical matrix that describes the frequency of terms that occur in a collection of documents
# In a document-term matrix, rows correspond to documents in the collection and columns correspond to terms.
suppressWarnings(rm(fb_tdm))
fb_tdm = TermDocumentMatrix(fb_corpus)

# convert as matrix
suppressWarnings(rm(fb_tdm1))
fb_tdm = as.matrix(fb_tdm)
fb_tdm1 <- fb_tdm[nchar(rownames(fb_tdm)) < 11,]

# add column names
colnames(fb_tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(fb_tdm1) <- colnames(fb_tdm)

cat("\n")
print(" Completed - Sentiment Analysis based on the Comparison Word Cloud using emotions")

##################################################
# Plotting  : Sentiments based on Facebook Data
##################################################

# Below is data processing for plotting sentiments
fb_plotdata <- Facebook_data[,c(11,16:23,25:34)]
colnames(fb_plotdata)

colnames(fb_plotdata) <- c("Date","Year","Month","Quarter","Polarity","SyuzhetScore","BingScore" ,
                           "AfinnScore","NrcScore","Anger","Anticipation","Disgust","Fear","Joy",
                           "Sadness","Surprise","Trust","Positive","Negative")

fb_plotdata <- fb_plotdata %>% filter( Date >= '2014-10-01' & Date <= '2017-09-30')
fb_plotdata <- fb_plotdata %>% mutate( YearQuarter = paste0(substr(as.character(Year),3,4),'Q',Quarter))

table(fb_plotdata$YearQuarter, fb_plotdata$Polarity)
#       Neutral Positive Negative
# 14Q4    1044      224       77
# 15Q1    1788      681      178
# 15Q2    2222      983      163
# 15Q3    1115      885      122
# 15Q4    1064      425      167
# 16Q1     200      155       71
# 16Q2     446      272       63
# 16Q3     316      122       47
# 16Q4     188      121       36
# 17Q1      71       52        2
# 17Q2      43       16        1
# 17Q3      17        8        3

prop.table(table(fb_plotdata$YearQuarter, fb_plotdata$Polarity),1)
#         Neutral   Positive   Negative
# 14Q4 0.77620818 0.16654275 0.05724907
# 15Q1 0.67548168 0.25727238 0.06724594
# 15Q2 0.65973872 0.29186461 0.04839667
# 15Q3 0.52544769 0.41705938 0.05749293
# 15Q4 0.64251208 0.25664251 0.10084541
# 16Q1 0.46948357 0.36384977 0.16666667
# 16Q2 0.57106274 0.34827145 0.08066581
# 16Q3 0.65154639 0.25154639 0.09690722
# 16Q4 0.54492754 0.35072464 0.10434783
# 17Q1 0.56800000 0.41600000 0.01600000
# 17Q2 0.71666667 0.26666667 0.01666667
# 17Q3 0.60714286 0.28571429 0.10714286


uniform_panels <- theme(panel.background = element_blank(), 
                        legend.key = element_blank(), 
                        legend.background=element_blank(), 
                        strip.background = element_blank())

# Ploarity plot quarter on quarter
ggplot(data = fb_plotdata, aes(x = YearQuarter, fill = Polarity )) + 
  geom_bar() + uniform_panels +theme(
    axis.title.x = element_text(color="black",face="bold", size=12),
    axis.title.y = element_text(color="black",face="bold", size=14),
    axis.text = element_text(color="black",size = 8),
    title = element_text(color="black",face="bold", size=14)
    )+theme(
      legend.position = c(0.9,0.5),
      legend.title = element_text(face="bold",size=10), 
      legend.text = element_text(color="black",size = 8))+
  scale_fill_manual(values = c("skyBlue","lightGreen","#e41a1c"))+
  labs(title="Polarity trend : Kurkure",x="Year and Quarter",y="word count")


# Emotion plot : quarter on quarter
library(reshape2)
library(dplyr)
emotion_data <- fb_plotdata[,c(10:17,20)] %>% group_by(YearQuarter) %>% summarise_all(sum)
emotion_data <- melt(emotion_data)
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
  labs(title="Emotions trend : Kurkure",x="Year and Quarter",y="Percentage")


#-----------------------------
# Sentiment plot
#
# Facebook : Plot the different sentiments from different methods
#-----------------------------------
fb_plotdata <- fb_plotdata %>% mutate( YearMonth = paste0(substr(as.character(Year),3,4),'M',Month))
fb_SentiScore_plotdata <- fb_plotdata[,c(6:9,21)] %>% group_by(YearMonth) %>% summarise_all(sum)

salesFile <- paste0(Inputdirectory,"Kurkure_SalesVolumeData.csv")

sales <- read.csv(salesFile, header = TRUE, sep= '\t' ,stringsAsFactors = FALSE)
colnames(sales)[1]<- "Date"
sales$DelhiandNCR<-round(as.numeric(gsub(",","",sales$DelhiandNCR)))
sales$Kolkata<-round(as.numeric(gsub(",","",sales$Kolkata)))
sales$Mumbai<-round(as.numeric(gsub(",","",sales$Mumbai)))
sales$Hyderabad<-round(as.numeric(gsub(",","",sales$Hyderabad)))
sales$Chennai<-round(as.numeric(gsub(",","",sales$Chennai)))
sales$TotalSalesVolume <- round(rowSums(sales[,c(2:6)]))

fb_SentiScore_plotdata <- cbind(fb_SentiScore_plotdata,sales)
fb_SentiScore_plotdata <- fb_SentiScore_plotdata[,c(-6:-11)]

dat <- data.frame(scale(fb_SentiScore_plotdata[,-1]))
dat <- cbind(fb_SentiScore_plotdata$YearMonth,dat)
dat <- melt(dat)
colnames(dat)[1] <- "YearMonth"
n = nrow(fb_SentiScore_plotdata)
x_lables = fb_SentiScore_plotdata[seq(1, n,3),1]


ggplot(data=dat,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="",y="")+
  theme(panel.background = element_blank(),
                    panel.grid.major = element_line(colour = "lightgrey"),
                    panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12,angle=45),
    axis.title.y = element_text(color="black",face="bold", size=14),
    axis.text = element_text(color="black",size = 8),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete(breaks = x_lables, labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9,0.8),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('#e41a1c',"#377eb8","#ff7f00","darkgreen","#f781bf"))


# let us plot Syuzhet Score and Sales Volume

dat_Syuzhet <- data.frame(scale(fb_SentiScore_plotdata[,c(-1,-3:-5)]))
dat_Syuzhet <- cbind(fb_SentiScore_plotdata$YearMonth,dat_Syuzhet)
dat_Syuzhet <- melt(dat_Syuzhet)
colnames(dat_Syuzhet)[1] <- "YearMonth"

ggplot(data=dat_Syuzhet,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="",y="")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12,angle=45),
    axis.title.y = element_text(color="black",face="bold", size=14),
    axis.text = element_text(color="black",size = 8),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete(breaks = x_lables, labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9,0.8),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('blue',"green"))


# let us plot BingScore and Sales Volume

dat_Bing <- data.frame(scale(fb_SentiScore_plotdata[,c(-1,-2, -4,-5)]))
dat_Bing <- cbind(fb_SentiScore_plotdata$YearMonth,dat_Bing)
dat_Bing <- melt(dat_Bing)
colnames(dat_Bing)[1] <- "YearMonth"

ggplot(data=dat_Bing,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="",y="")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12,angle=45),
    axis.title.y = element_text(color="black",face="bold", size=14),
    axis.text = element_text(color="black",size = 8),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete(breaks = x_lables, labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9,0.8),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('blue',"green"))

# let us plot AfinnScore and Sales Volume

dat_Afinn <- data.frame(scale(fb_SentiScore_plotdata[,c(-1,-2, -3,-5)]))
dat_Afinn <- cbind(fb_SentiScore_plotdata$YearMonth,dat_Afinn)
dat_Afinn <- melt(dat_Afinn)
colnames(dat_Afinn)[1] <- "YearMonth"

ggplot(data=dat_Afinn,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="",y="")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12,angle=45),
    axis.title.y = element_text(color="black",face="bold", size=14),
    axis.text = element_text(color="black",size = 8),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete(breaks = x_lables, labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9,0.8),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('blue',"green"))

# let us plot nrcScore and Sales Volume

dat_nrc <- data.frame(scale(fb_SentiScore_plotdata[,c(-1:-4)]))
dat_nrc <- cbind(fb_SentiScore_plotdata$YearMonth,dat_nrc)
dat_nrc <- melt(dat_nrc)
colnames(dat_nrc)[1] <- "YearMonth"

ggplot(data=dat_nrc,
       aes(x=YearMonth, y=value, colour=variable)) +
  geom_line(aes(group = variable),size=1,linetype = "solid")+
  labs(title="Sentiment and Sales Volume trend : Kurkure",x="",y="")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor =  element_line(colour = "lightgrey")) +
  theme(
    axis.title.x = element_text(color="black",face="bold", size=12,angle=45),
    axis.title.y = element_text(color="black",face="bold", size=14),
    axis.text = element_text(color="black",size = 8),
    title = element_text(color="black",face="bold", size=10)
  )+
  scale_x_discrete(breaks = x_lables, labels = x_lables)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9,0.8),
    legend.text = element_text(color="black",size = 8))+
  scale_colour_manual(values = c('blue',"green"))



# Facebook : Plot comparison wordcloud 
comparison.cloud(fb_tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(0.9, 0.4),rot.per=0.4)

# Facebook : Plot commonality cloud
commonality.cloud(fb_tdm1, random.order=FALSE, colors = brewer.pal(8, "Dark2"))

v <-sort(rowSums(fb_tdm1),decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word, d$freq, max.words = 100, random.order = FALSE,colors = brewer.pal(8,"Dark2"))

rm(v)
rm(d)
rm(result)
rm(fb_tdm)
rm(fb_tdm1)
rm(trust_pattern_pos)
rm(fear_pattern_pos)
rm(fb_sentiments)
rm(fb_emotions)
rm(fb_all)
rm(fear_emotion)
rm(trust_emotion)
rm(fb_emo_per)
rm(Facebook_posts)
rm(emo_bar)
rm(fb_corpus)
rm(fb_meanscore)
rm(fb_afinn)
rm(fb_bing)
rm(fb_nrc)
rm(fb_syuzhet)
rm(hu.liu.neg)
rm(hu.liu.pos)
rm(fear_updated)
rm(trust_updated)
rm(polarity)

rm(p)
rm(syuzhet_method)
rm(bing_method)
rm(afinn_method)
rm(nrc_method)
rm(fb_sentiment_score)
rm(df)



