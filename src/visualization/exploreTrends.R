
# DATE: 12.09.2017


library(plyr)
library(tidyverse)
library(stringr)
library(stringi) 
library(openssl)
library(lubridate)

# For this project, we will use the myRtweet Package.  This is just a modification of the 
# rtweet package, that allows access to the Premium Twitter Search API

use_package <- "./myRtweet"    # On UNIX and Windows

devtools::load_all(path = use_package)
devtools::document(pkg = use_package)



source("R Code/TwitterCode/TwitterUtilityFuncs.R")
source("R Code/FBCode/FaceBookUtilityFuncs.R")



#########################################################################################
########################## Download & Pre-process Data ##################################
#########################################################################################

#### Twitter #### -----------------------------------------------------------------------

if (!exists("cleanPolTL", inherits = FALSE)) cleanPolTL <- get_PolTimeline()
minDate <- as.Date(min(cleanPolTL$dateCreated))
maxDate <- as.Date(max(cleanPolTL$dateCreated))
numDays <- as.integer(maxDate - minDate)

dateLookUp <- ldply(0:numDays, function(d) {
  useDate <- minDate + d
  useID <- get_BoundaryID(useDate, polTL = cleanPolTL, includeDF = FALSE)
  useMinID <- useID$minID
  useMaxID <- useID$maxID
  
  if (is_empty(useMaxID)) {
    maxLen <- max(nchar(cleanPolTL$id), nchar(useMinID), 0, na.rm = TRUE)
    newDF <- mutate(cleanPolTL, padded_status_id = pad_id(id, maxLen)) 
    useMinID <- pad_id(useMinID, maxLen)
    
    useMaxID <- max(newDF$padded_status_id, useMinID, na.rm = TRUE)  # the max ID in the current data set
  }

  result <- tibble(Date = useDate, minID = useMinID, maxID = useMaxID)
  return(result)
})

add_Date <- function(currID, lookUp) {
  maxLen <- max(nchar(lookUp$minID), nchar(lookUp$maxID), nchar(currID))
  lookUp <- mutate(lookUp, minID = pad_id(minID, maxLen), maxID = pad_id(maxID, maxLen))
  l <- nrow(lookUp)
  
  absMin <- lookUp$minID[1]
  absMax <- lookUp$maxID[l]
  
  gDate <- ldply(currID, function(i) {
    res <- lookUp %>%
      filter(minID < i, maxID > i) %>%
      pull(Date)
    
    if (is_empty(res)) {
      if (i < absMin) res <- NA
      if (i >= absMax) res <- lookUp$Date[l]
    }
    return(res)
  })
  return(gDate[[1]])
}

cleanPolTL <- mutate(cleanPolTL, Date = add_Date(cleanPolTL$id, dateLookUp))
TwPolPosts <- select(cleanPolTL, Politician, text, Date, favoriteCount, shareCount = retweetCount) %>%
  mutate(Site = "Twitter")

#### Facebook #### ----------------------------------------------------------------------
if (!exists("cleanedFBPosts", inherits = FALSE)) cleanedFBPosts <- get_PolFeed(returnRes = "Feed")

cleanedFBPosts <- mutate(cleanedFBPosts, Date = as.Date(dateCreated))
FBPolPosts <- select(cleanedFBPosts, Politician, text, Date, favoriteCount, shareCount) %>%
  mutate(Site = "Facebook")

PolPosts <- bind_rows(TwPolPosts, FBPolPosts)
infoDF <- get_AllPolInfo()

PolPosts <- left_join(PolPosts, infoDF, by = c("Politician" = "listName"))

### Lets examine Leader Politicians:

leaderDF <- filter(PolPosts, Funktion == "Leader")

partyColors <- c("SPÖ" = "red1", "ÖVP" = "turquoise1", "FPÖ" = "blue", "NEOS" = "deeppink", 
                 "GRÜNE" = "green3", "LISTE JETZT" = "grey", "KPÖ" = "red4", "WANDEL" = "purple")

(polLabels <- unique(df1$Name))
(polColors <- c("red2", "turquoise1", "blue", "deeppink", "green3", "grey", "red4", "purple"))
names(polColors) <- polLabels



df1 <- leaderDF

gDF <- group_by(leaderDF, Name, Date, Site) %>%
  summarize(numPosts = n())

ggplot(gDF, aes(x = Date, y = numPosts, color = factor(Name))) +
  geom_line() +
  facet_wrap(~Site) +
  scale_color_manual(values = polColors) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(color = "Politician") +
  ylab("Number of Posts") 

ggplot(df1, aes(x = Date, y = favoriteCount, color = factor(Name))) +
stat_summary(fun.y = sum, na.rm = TRUE, geom ='line') +
  scale_color_manual(values = polColors, labels = polLabels)

ggplot(df1, aes(x = Date, y = favoriteCount, color = factor(Name))) +
  stat_summary(fun.y = sum, na.rm = TRUE, geom ='line') +
  scale_color_manual(values = polColors) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~Site) +
  labs(color = "Politician") +
  ylab("Favorite Count")


ggplot(df1, aes(x = Date, y = shareCount, color = factor(Name), group = factor(Name))) +
  stat_summary(fun.y = sum, na.rm = TRUE, geom ='line') +
  scale_color_manual(values = polColors) +
  labs(color = "Politician") +
  ylab("Number of Shares")


ggplot(df1, aes(x = Date, y = favoriteCount + shareCount)) +
  stat_summary(aes(color = Politician, group = Politician), fun.y = sum, na.rm = TRUE, geom ='line')


ggplot(df1, aes(x = Politician, weight = shareCount, fill = Politician)) +
  geom_bar() +
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none")


ggplot(df1, aes(x = Name, weight = favoriteCount, fill = factor(Name))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none") +
  scale_fill_manual(values = polColors, labels = polLabels) +
  ggtitle("Number of likes in one week")

ggplot(df1, aes(x = Name, weight = shareCount, fill = factor(Name))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none") +
  scale_fill_manual(values = polColors) +
  ggtitle("Number of shares in one week")


### The most ...

topFav <- arrange(PolPosts, desc(favoriteCount)) %>%
  .[1:10, ]

topShare <- arrange(PolPosts, desc(shareCount)) %>%
  .[1:10, ]
