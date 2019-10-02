

# DATE: 12.09.2019
# Downloading data from Twitter using rtweet
# Starting September 8th, ending posts on Sept. 30th, and responses on Oct. 4th


# General Twitter usage tips:
# Go to developer.twitter.com
# NRW2019watch account approved

# Hints on setting up Authentication:
# http://thinktostart.com/twitter-authentification-with-r/
# Callback URL: http://127.0.0.1:1410

# Info on rtweet:
# https://github.com/mkearney/rtweet
# https://rtweet.info/


# This twitter website is also interesting:
# https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-lookup

# We have to alter the search_term code to allow premium search options:
# https://developer.twitter.com/en/docs/tweets/search/api-reference/premium-search#SearchRequests



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

# Stable Politician List:  there are 315 Twitter accounts

# We want to track all posts and comments from Sept. 8, 2019, 
# until Sept. 30, 2019 (posts) or Oct. 4, 2019 (comments)



#########################################################################################
######  Get the Comments to Politicians #################################################
#########################################################################################


#### Combining Tweets from Standard Search #### -----------------------------------------

# explicitly load the Twitter Token, in order to control which app is being used for downloading the data
TwToken <- readRDS("Data/TwData/Tokens/Token.RDS")


# Keep track of the dates downloaded so far
if (file.exists("Data/TwData/DownloadDates.RData")) {
(datesDownloaded <- readRDS("Data/TwData/DownloadDates.RData"))
} else {
  (datesDownloaded <- c())
}

fromDay <- max(datesDownloaded + 1, firstDay)  # date to start collecting from
untilDate <- min(fromDay + 2, Sys.Date() - 1)  # date to end collecting (not included)

sink(file = paste0("Data/TwData/TwLogs/", as.integer(Sys.time()), "_logfile.txt"), split = TRUE)  # save to a logfile

updateAtComments <- update_atComments(polDF = NULL, polTml = NULL, startDay = fromDay, stopDay = untilDate,
                              sTerms = NULL, listNames = NULL, 
                              numComments = 50000, morePols = morePols, 
                              fName = "CleanedtoComments.RData", newToCom = NULL)




sink()


# update dates downloaded so far
(datesDownloaded <- unique(c(datesDownloaded, fromDay + 0:as.integer(untilDate - fromDay - 1))))
saveRDS(datesDownloaded, file = "Data/TwData/DownloadDates.RData")
