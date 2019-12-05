
library(plyr)
library(tidyverse)
library(stringr)
library(stringi) 
library(rdrop2)
library(openssl) # For hashing
library(lubridate)

# For this project, we will use the myRtweet Package.  This is just a modification of the 
# rtweet package, that allows access to the Premium Twitter Search API

use_package <- "./myRtweet"    # On UNIX and Windows

devtools::load_all(path = use_package)
devtools::document(pkg = use_package)


# source the Twitter utility functions
source("R Code/TwitterCode/TwitterUtilities.R")

# explicitly load the Twitter Token, in order to control which app is being used for downloading the data
TwToken <- readRDS("Data/TwData/Tokens/Token.RDS")

# Load the list of keywords
kwDF <- read_csv("PoliticianList/KeyWords.csv")

# what is the maximum number of comments to download
maxComs <- 50000

# starting at what minimum ID?
if (file.exists("PoliticianList/TwStartID.RDS")) {
  minID <- readRDS("PoliticianList/TwStartID.RDS")
} else minID <- "0"


# 
N <- nrow(kwDF)


##### Info needed to filter out only those comments that relate to Austria ...

polDF <- get_AllPolInfo()
contains1 <- "austria|österreich|oesterreich"
contains2 <- polDF$Party %>%
  str_to_lower() %>%
  setdiff(c(NA, "jetzt")) %>%
  unique() %>%
  paste(collapse = "|")
contains3 <- polDF$Name[1:71] %>%
  str_to_lower() %>%
  setdiff(NA) %>%
  unique() %>%
  paste(collapse = "|")
contains4 <- polDF$Name[72:142] %>%
  str_to_lower() %>%
  setdiff(NA) %>%
  unique() %>%
  paste(collapse = "|")




### lets download the comments in 5 rounds:
rounds <- seq(1, N, length.out = 6)

for (j in 1:5) {
  kwRES <- tibble()
  fName <- paste0("tempKW_", j, ".RData")
  fPath <- file.path("Data", "TwData", "TwLogs", fName)
  
  for (i in rounds[j]:rounds[j+1]) {
    Topic <- kwDF$TOPIC[i]
    kw <- kwDF$KEYWORD[i]
    
    res <- search_tweets(kw, n = maxComs, min_id = minID, token = TwToken)
    if (!(is_empty(res) || nrow(res) == 0)) {
      res <- mutate(res,
                    Topic = Topic,
                    Keyword = kw)
    }
    kwRES <- bind_rows(kwRES, res)
    if (i %% 5 == 0L) save(kwRES, file = fPath)
  }
  
  save(kwRES, file = fPath)
  

  # filter out only those comments related to Austria, and save separately by Topic  
  Topics <- unique(kwRES$Topic)
  
  for (t in Topics) {
    print(t)
    fName <- paste0(t, "_KW.RData")
    fPath <- file.path("Data", "TwData", "KeyWords", fName)
    if (file.exists(fPath)) {
      load(fPath)
    } else topicRes <- NULL
    
    topicRes1 <- filter(kwRES, Topic == t)
    
    if (!is_empty(topicRes1)) {
      topicResAT <- filter(topicRes1, country == "Austria")
      topicResOther <- filter(topicRes1, !(status_id %in% topicResAT$status_id))
      
      useText <- str_to_lower(topicResOther$text)
      keepKW <- str_detect(useText, contains1) | str_detect(useText, contains2) | str_detect(useText, contains3) | str_detect(useText, contains4)
      topicResAT <- filter(topicResOther, keepKW) %>%
        bind_rows(topicResAT)
      
    }
    
    if (!is_empty(topicResAT)) {
      
      topicRes <- topicRes %>%
      {
        if (!is_empty(.))  filter(., !(status_id %in% topicResAT$status_id))
      } %>%
        bind_rows(topicResAT)
      
      save(topicRes, file = fPath)
    }
  }
  
}