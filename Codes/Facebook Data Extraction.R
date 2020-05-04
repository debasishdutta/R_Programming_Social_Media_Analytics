#############################################################################################
####################### Post Extraction From Specific Facebook Pages ########################
#############################################################################################
############# Developer:  Debasish Dutta                                        ############# 
############# Date:       July 2017                                             #############
############# Input:      1. Names of Facebook Pages As Character Vector        #############
#############             (https://www.facebook.com/xxxx/)                      #############
#############             xxxx Has To Be A Facebook Page Name (No User/ Group Name) #########
#############             2. Facebook Authentication Object Name As Character   #############
#############             3. Max Count of Post                                  #############
#############################################################################################
facebook_extraction <- function(page_names,auth_file, max_posts){

################ Installing & Loading Rfacebook Package ################
if (!require("Rfacebook", character.only = TRUE))
{
  install.packages("Rfacebook", dep = TRUE)
  if (!require("Rfacebook", character.only = TRUE))
    stop("Rfacebook Package not found")
}

################ Loading Facebook Authentication Object ################
load(auth_file)

################ Facebook Comment Extraction ################
final_df <- data.frame()
for (i in page_names)
{
  fb_page <- try(getPage(page=i, 
                         token=fb_authentication,
                         n=max_posts,
                         feed = TRUE,
                         reactions = TRUE,
                         verbose = TRUE), silent = TRUE)
  
  if (class(fb_page) == "try-error"){
    temp_data <- NULL
  }else{
    temp_data <- fb_page
  }
  final_df <- rbind(final_df, temp_data)
}

final_df <- final_df[,c(c("from_name", "created_time", 
                          "type", "message", 
                          "likes_count", "comments_count", 
                          "shares_count", "love_count", 
                          "haha_count", "wow_count", 
                          "sad_count", "angry_count"))]
names(final_df) <- c("Page_Name", "Posting Date", "Post_Type",
                     "Message","No_Likes", "No_Comments",
                     "No_Shares", "Emotion_Love",
                     "Emotion_Haha", "Emotion_Wow",
                     "Emotion_Sad", "Emotion_Angry")

############### Cleaning The Text ###############
final_df$Message <- iconv(final_df$Message, "latin1", "ASCII", sub="")
final_df$Message = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", final_df$Message) 
final_df$Message = gsub("@\\w+", "", final_df$Message)    
final_df$Message = gsub("[[:punct:]]", "", final_df$Message) 
final_df$Message = gsub("[[:digit:]]", "", final_df$Message)
final_df$Message = gsub('[[:cntrl:]]', '', final_df$Message)
final_df$Message = gsub("http\\w+", "", final_df$Message) 
final_df$Message = gsub('\\d+', '', final_df$Message)
final_df$Message = gsub("[ \t]{2,}", "", final_df$Message) 
final_df$Message = gsub("^\\s+|\\s+$", "", final_df$Message) 

return(final_df)
}
