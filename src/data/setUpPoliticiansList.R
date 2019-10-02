
# DATE: 12.09.2019

# This script is for initial use only:
# set up the politicians list for regular usage in the twitter and FB collection

# Start data collection on Sept 8, 2019
# End post collection on Sept. 30, 2019
# End comment/reply collection on Oct. 4, 2019

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
############## CLEAN UP LIST OF POLITICIANS TO FOLLOW ###################################
#########################################################################################

newPolDF <- prep_PolDF(fName = "List_of_Politicians.csv")


#########################################################################################
################### INITIALIZE DATA COLLECTION ##########################################
#########################################################################################

###################### Twitter ###########################################

# Twitter requires a special first run, because Twitter API searches are by 
# Tweet ID, and not by Date.  Therefore, in order to control the start date
# we need to run a preliminary timeline download, which we will use to determine 
# the Tweet ID corresponding to the given start Date.
# In addition, not all tweets are returned with a created at time.

polDF <- get_AllPolDF("Tw")

# explicitly load the Twitter Token, in order to control which app is being used for downloading the data
TwToken <- readRDS("Data/TwData/Tokens/Token.RDS")
usePolHandles <- polDF$polHandles
temp <- lookup_users(usePolHandles, token = TwToken)

# Quick way to check who still has Twitter Account:
noTwitter <- setdiff(str_to_lower(usePolHandles), str_to_lower(temp$screen_name))

#### Check the timelines:

# Run through all politician handles, collecting their timelines.
# By setting useMin = TRUE & minimumID = "0", we tell it to
# find and save the new minimum ID corresponding to September 8, 2019 (limitDate)

update_PolTimelines(polDF = NULL,
                    morePols = NULL,
                    numPosts = 500,
                    useMin = TRUE,
                    minimumID = "0",
                    useDate = Sys.Date() - 2,
                    fName = "CleanedPolTimeLines") 




###################### FaceBook ##########################################

# No special first set up necessary, because Facebook data collection
# always follows the scheme 
# 1. collect posts
# 2. collect comments to posts
# 3. collect replies to comments

# FB API allows search by Date, and all entries are returned with a created at time
