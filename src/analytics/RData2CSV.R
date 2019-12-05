
# Turn .RData DF's into CSV's
#  For the sake of brevity, we will refer to the Politicians/Influencers/Political Parties/Media
#  being monitored for this social media project simply as politicians


library(plyr)
library(tidyverse)
library(stringr)
library(stringi) 
library(rdrop2)
library(openssl)
library(lubridate)

# DropBox Info:
# httr-oauth created that uses the NRW2019 Team DropBox, data 4good Folder

source("R Code/AnalyseUtilities.R")


#########################################################################################
###################### PRE_PROCESS POLITICIAN TIMELINES #################################
#########################################################################################

#### Twitter #### -----------------------------------------------------------------------

if (!exists("TwPolTL", inherits = FALSE)) TwPolTL <- get_PolTimeline()

if (any(is.na(TwPolTL$dateCreated))) {
  dateLookUp <- create_DateDictionary(TwPolTL)
  TwPolTL <- mutate(TwPolTL, Date = add_Date(TwPolTL$id, dateLookUp))
} else {
  TwPolTL <- mutate(TwPolTL, Date = as.Date(dateCreated))
}

TwPolTL$Level <- 0L

names(TwPolTL)
# [1] "user"                   "userID"                 "text"                   "id"                     "replyToUser"           
# [6] "replyToID"              "dateCreated"            "favoriteCount"          "retweetCount"           "verified"              
# [11] "isQuote"                "quoted_text"            "quoted_screen_name"     "quoted_status_id"       "quoted_favorite_count" 
# [16] "quoted_retweet_count"   "quoted_verified"        "isRetweet"              "retweet_text"           "retweet_screen_name"   
# [21] "retweet_status_id"      "retweet_favorite_count" "retweet_retweet_count"  "retweet_verified"       "source"                
# [26] "Politician"             "Sampled"                "Date"                   "Level"                       


#### Facebook #### ----------------------------------------------------------------------

if (!exists("FBPosts", inherits = FALSE)) FBPosts <- get_PolFeed(returnRes = "Feed")

FBPosts <- mutate(FBPosts, 
                  Date = as.Date(dateCreated),
                  dateCreated = ymd_hms(dateCreated))
                  
  

(useVars <- intersect(names(FBPosts), names(TwPolTL)))


TwPolPosts <- select(TwPolTL, one_of(useVars), shareCount = retweetCount, -Sampled) %>%
  mutate(Site = "Twitter")

FBPolPosts <- select(FBPosts, one_of(useVars), shareCount, Level, -Sampled) %>%
  filter(Level == 0L) %>%
  mutate(Site = "Facebook")

# Lets extract from here the Level 10 FB comments too ...
FBComsL10 <- filter(FBPosts, Level == 10L) %>%
  mutate(Site = "Facebook")

all(names(TwPolPosts) %in% names(FBPolPosts))

PolPosts <- bind_rows(TwPolPosts, FBPolPosts)
infoDF <- get_AllPolInfo()

PolPosts <- left_join(PolPosts, infoDF, by = c("Politician" = "listName")) %>%
  select(Name, user, userID, text, textID = id, replyToUser, replyToID, dateCreated, Level, favoriteCount, shareCount, Site, Type, Party, Funktion, Politician) %>%
  arrange(Politician, dateCreated)


save(PolPosts, file = "Data/ProcessedData/PolPosts.RData")
write_csv(PolPosts, path = "Data/CSVData/PolPosts.csv")




#########################################################################################
######################### PRE_PROCESS USER COMMENTS #####################################
#########################################################################################

#### Get the Twitter user comments #### -------------------------------------------------

if (!exists("TwComments", inherits = FALSE)) TwComments <- get_PolLevels()

TwCommentsDF <- bind_rows(TwComments, .id = "PolName") %>%
  select(PolName, everything(), -Politician) %>%
  rename(Politician = PolName, shareCount = retweetCount, Level = replyLevel) %>%
  mutate(Site = "Twitter",
         origPost = ifelse(Level == 10L, id, origPost))
  
any(is.na(TwCommentsDF$Politician))
any(is.na(PolPosts$Politician))
any(is.na(TwCommentsDF$origPost))

# what are the column headings?
names(TwCommentsDF)
# [1] "Politician"             "user"                   "userID"                 "text"                   "id"                    
# [6] "replyToUser"            "replyToID"              "dateCreated"            "favoriteCount"          "shareCount"            
# [11] "verified"               "isQuote"                "quoted_text"            "quoted_screen_name"     "quoted_status_id"      
# [16] "quoted_favorite_count"  "quoted_retweet_count"   "quoted_verified"        "isRetweet"              "retweet_text"          
# [21] "retweet_screen_name"    "retweet_status_id"      "retweet_favorite_count" "retweet_retweet_count"  "retweet_verified"      
# [26] "source"                 "Level"                  "origPost"               "Sampled"                "Site"  



#### Get the Facebook user comments #### ------------------------------------------------

if (!exists("FBComments", inherits = FALSE)) FBComments <- load_FBComments(getRep = TRUE)

(useVars <- intersect(names(FBComsL10), names(FBComments)))

FBCommentsDF <- select(FBComsL10, one_of(useVars)) %>%
  mutate(id = FBhash_id(id, user),
         origPost = FBhash_origPost(origPost, Level),
         shareCount = 0L) %>%
  bind_rows(FBComments) %>%
  mutate(Site = "Facebook", replyCount = NULL)

# what are the column headings?
names(FBCommentsDF)
# [1] "Politician"    "user"          "text"          "id"            "dateCreated"   "favoriteCount" "replyToID"     "origPost"     
# [9] "Level"         "Site" 

(useVars <- intersect(names(TwCommentsDF), names(FBCommentsDF)))

UserComments <- bind_rows(TwCommentsDF, FBCommentsDF) %>%
  rename(textID = id)

UserComments <- left_join(UserComments, infoDF, by = c("Politician" = "listName")) %>%
  select(Name, user, userID, text, textID, replyToUser, replyToID, origPost, dateCreated, Level, favoriteCount, shareCount, Site, Type, Party, Funktion, Politician) %>%
  arrange(Politician, dateCreated)

PolPosts <- mutate(PolPosts, origPost = textID)

setdiff(names(PolPosts), names(UserComments))

PostsComs <- bind_rows(PolPosts, UserComments)

# get counts of responses:
# replyCount = number of replies to a Post/Comment; 
# commentCount = number of comments in a thread originating with the current comment
# (applies only to Level 0 or Level 10: all others are set to 0)

PostsComs1 <- count_Comments(PostsComs)

PolPosts <- filter(PostsComs1, Level == 0L)
UserComments <- filter(PostsComs1, Level > 0L)

save(PolPosts, file = "Data/ProcessedData/PolPosts.RData")
write_csv(PolPosts, path = "Data/CSVData/PolPosts.csv")
drop_upload("Data/CSVData/PolPosts.csv", path = "CSVData")

save(UserComments, file = "Data/ProcessedData/UserComments.RData")
write_csv(UserComments, path = "Data/CSVData/UserComments.csv")
drop_upload("Data/CSVData/UserComments.csv", path = "CSVData")


