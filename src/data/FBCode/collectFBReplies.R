
# DATE: 23.09.2019
# Facebook reply-to-comment collection
# This has to be supervised, due to FB API rate limits, which are undocumented and unpredictable


# Problems with FaceBook:
# Check out FB_Problems.R

library(plyr)
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(rdrop2)
library(Rfacebook)
library(lubridate)
library(openssl)

source("./R Code/FBCode/FaceBookUtilities.R")
source("./R Code/TwitterCode/TwitterUtilities.R")


set.seed(as.integer(Sys.time()))

#########################################################################################
####################  Get the Comments downloaded so far  ###############################
#########################################################################################

polDF <- get_AllPolDF("FB")

# which politicians to follow?
followPols <- polDF$listName

useFN <- get_fileNum(sDir = "Data/FBData", fPattern = "FBReps", nPattern = "\\d+", decr = FALSE)

startDate <- firstDay
minDate <- Sys.Date() - 7 - 2
maxFileNum1 <- as.integer(minDate - startDate) %/% 3 + 1

maxFileNum2 <- get_fileNum(sDir = "Data/FBData/ready4Sampling", fPattern = "FBComs", nPattern = "\\d+", decr = TRUE)

maxFileNum <- min(maxFileNum1, maxFileNum2)

#########################################################################################
########################  Sort the User Comments  #######################################
#########################################################################################

# names(comDF)
# "Politician"     "user"           "userID"         "text"           "id"             "replyToID"      "replyToUser"   
# "dateCreated"    "favoriteCount"  "comments_count" "origPost"       "Level"          "Include"        "repAvail"

# names(comDFavail)
# "Politician"     "user"           "id"             "origPost"       "replyToID"     
# "dateCreated"    "comments_count" "repAvail"


if (useFN <= maxFileNum) { # then we can do some comment collection:
  
list2env(get_DownloadedReplies(FN = useFN), envir = environment())
  
  
  
# Get replies for comments that are at most 6 days older than the comment
repAvail <- filter(comDFavail, !(Politician %in% followPols) | repAvail > as.Date(dateCreated) + 7)
  

RepToDownload <- filter(comDFavail, Politician %in% followPols, repAvail < as.Date(dateCreated) + 8) %>% 
  arrange(desc(comments_count))


StillToDownload <- NULL
if (!is_empty(RepToDownload)) {
  
  noReplies <- filter(RepToDownload, comments_count == 0) %>%
    mutate(repAvail = as.Date("2020-12-31"))
  
  repAvail <- bind_rows(repAvail, noReplies)
  
  StillToDownload <- filter(RepToDownload, comments_count > 0)
}

if (is_empty(StillToDownload)) l <- 0 else l <- nrow(StillToDownload)

if (nrow(repAvail) + l == nrow(comDFavail)) {
  save(repAvail, file = "Data/FBData/TempR/availableReplies.RData")
  rm(RepToDownload, repAvail, noReplies, comDFavail)
}

allDownloaded <- FALSE

#########################################################################################
###################  Download Replies to User Comments  #################################
#########################################################################################


# what is the max number of comments to download per post??
maxNum <- 24950  # FB does not allow more!!

# To get All:
firstNR <- 1 
maxComs <- l

# Or get some at a time:
firstNR <- 10440
maxComs <- l


numComs <- maxComs - firstNR + 1

# Figure out how many tokens we have, so we can determine how many comments to download per round
d <- dir("Data/FBData/Tokens")
T <- length(d) # Number of tokens available

# check which tokens are currently working
ERROR <- FALSE
useTokens <- NULL
for (i in 1:T) {
  FBToken <- load_Token(i)
  info <- tryCatch(getUsers("me", token = FBToken),
                   error = function(e) {
                     print(e)
                     ERROR <- TRUE
                   })
  if (!ERROR) useTokens <- c(useTokens, i)
  ERROR <- FALSE
}

T <- length(useTokens)

useTokens <- sample(useTokens, T)
perRound <- ceiling(numComs/T)

# useTokens <- c(useTokens, useTokens)
# perRound <- ceiling(numComs/(2*T))



# keep a log of messages
sink(file = "Data/FBData/TempR/logFile.txt", split = TRUE)  

comReps <- NULL

totalComments <- 0


for (k in useTokens) {
  # Decide which FB Token to use ...
  FBToken <- load_Token(k)
  
  print("***************************************")
  print(paste0("Loaded Token ", k))
  print(" ")
  
  # # or using GraphAPI (lasts only 2 hours)
  # FBToken <- "<fill in FB token here>"

  startI <- min(firstNR, maxComs)
  endI <- min(maxComs, firstNR + perRound - 1)
  
  for (i in startI:endI) {
    print(i)
    out <- StillToDownload[i, ]
    numComments <- out$comments_count
    if (numComments > 0) {
      totalComments <- totalComments + numComments
      origPost <- out$origPost
      print(paste0("inside all_replies, original Post is ", origPost, " and i is: ", i))
      
      res <- tryCatch(get_FBReplies(out, 
                                    numRep = 10000,    # indicate a maximum number of replies to get
                                    untilDate = as.Date(out$dateCreated) + 7),   
                      error = function(e) {
                        msg <- as.character(simpleError(e))
                        print(msg)
                        return(NULL)
                      })
      if (!is_empty(res)) {
        comReps <- bind_FBdfs(comReps, res)
        StillToDownload$repAvail[i] <- Sys.Date()
        
        if (i %% 10 == 0L) {
          save(comReps, file = "Data/FBData/TempR/Replies.RData")
          Sys.sleep(10)
        }
      } 
      
      # Take a break!
      if (i %% 100 == 0) {
        if (i %% 500 == 0) {
          totalComments <- 0
          save(comReps, file = "Data/FBData/TempR/Replies2.RData")
          print("taking a few minutes break")
          Sys.sleep(1200)
        } else {
          Sys.sleep(180)
        }
      } else if (totalComments > 1000) {
        totalComments <- 0
        save(comReps, file = "Data/FBData/TempR/Replies2.RData")
        Sys.sleep(720)
      }
    }
    
    Sys.sleep(1)
    print("************************************")
    
  }
  
  if (!is.null(comReps)) {
    comReps <- filter(comReps, !(text %in% c("", " "))) %>%  # get rid of empty messages
      replace_na(list(comments_count = 0))
    save(comReps, file = "Data/FBData/TempR/Replies2.RData")
  }
  
  firstNR <- endI + 1
  if (firstNR > maxComs) break
  
  print("waiting 30 minutes before loading next token")
  Sys.sleep(1500)
}

sink()

# if all the errors are of type "Unsupported get request", we can get rid of the errorLogR
errors <- bind_rows(errorLogR)
if (all(str_detect(errors$Msg, "Unsupported get request"))) errorLogR <- list()


#### DON'T HASH THE COMMENT ID's!!!!! Wait until after sampling! #####
repDF <- comReps %>%
  clean_FBCommentsDF(polDF = polDF, morePols = morePols, Level = NULL) %>%
  bind_FBdfs(dfOld = repDF) %>%
  unique()

load("Data/FBData/TempR/availableReplies.RData")
comDFavail <- bind_rows(StillToDownload, repAvail)

### Update firstNR, and save:
if (firstNR > nrow(StillToDownload)) {
  firstNR <- 1
  allDownloaded <- TRUE
}



save_Rep(comDFavail, repDF, errorLogR, allDone = allDownloaded, FN = useFN)


# If no errors occurred, clear the Temp Directory
if (is_empty(errorLogR)) clean_Directory("Data/FBData/TempR")

}


