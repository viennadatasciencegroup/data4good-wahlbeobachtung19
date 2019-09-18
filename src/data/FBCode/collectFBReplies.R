
# DATE: 22.03.2019
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

source("./R Code/FBCode/FaceBookUtilityFuncs.R")
source("./R Code/TwitterCode/TwitterUtilityFuncs.R")



#########################################################################################
####################  Get the Comments downloaded so far  ###############################
#########################################################################################

polDF <- get_AllPolDF("FB")

# which politicians to follow?
followPols <- update_usedPols(Site = "FB", maxPols = NULL)  # used to be 45 prior to FN 11
availPols <- polDF$listName
followPols <- intersect(followPols, availPols)



useFN <- get_fileNum(sDir = "Data/FBData", fPattern = "FBReps", nPattern = "\\d+", decr = FALSE)

startDate <- as.Date("2019-04-15")
minDate <- Sys.Date() - 10 - 5
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
    mutate(repAvail = as.Date("2019-12-31"))
  
  repAvail <- bind_rows(repAvail, noReplies)
  
  StillToDownload <- filter(RepToDownload, comments_count > 0)
}

if (is_empty(StillToDownload)) l <- 0 else l <- nrow(StillToDownload)

if (nrow(repAvail) + l == nrow(comDFavail)) {
  save(repAvail, file = "Data/TempR/availableReplies.RData")
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
firstNR <- 16800
maxComs <- l


numComs <- maxComs - firstNR + 1
perRound <- ceiling(numComs/7)


# keep a log of messages
sink(file = "Data/TempR/logFile.txt", split = TRUE)  

comReps <- NULL

totalComments <- 0


for (k in 1:7) {
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
                        errID <- out$id
                        pol <- out$Poltician
                        errorInfo <- list(Pol = pol, id = errID, Msg = msg, Time = Sys.time())
                        errorLogR[[errID]] <<- errorInfo
                        return(NULL)
                      })
      if (!is_empty(res)) {
        comReps <- bind_FBdfs(comReps, res)
        StillToDownload$repAvail[i] <- Sys.Date()
        
        if (i %% 10 == 0L) {
          save(comReps, file = "Data/TempR/Replies.RData")
          Sys.sleep(5)
        }
      } 
      
      # Take a break!
      if (i %% 100 == 0) {
        if (i %% 1000 == 0) {
          totalComments <- 0
          save(comReps, file = "Data/TempR/Replies2.RData")
          print("taking a few minutes break")
          Sys.sleep(600)
        } else {
          Sys.sleep(45)
        }
      } else if (totalComments > 5000) {
        totalComments <- 0
        save(comReps, file = "Data/TempR/Replies2.RData")
        Sys.sleep(400)
      }
    }
    
    Sys.sleep(1)
    print("************************************")
    
  }
  
  if (!is.null(comReps)) {
    comReps <- filter(comReps, !(text %in% c("", " "))) %>%  # get rid of empty messages
      replace_na(list(comments_count = 0))
    save(comReps, file = "Data/TempR/Replies2.RData")
  }
  
  firstNR <- endI + 1
  if (firstNR > maxComs) break
  
  print("waiting 10 minutes before loading next token")
  Sys.sleep(600)
}

sink()


#### DON'T HASH THE COMMENT ID's!!!!! Wait until after sampling! #####
repDF <- comReps %>%
  clean_FBCommentsDF(polDF = polDF, morePols = morePols, Level = NULL) %>%
  bind_FBdfs(dfOld = repDF) %>%
  unique()

load("Data/TempR/availableReplies.RData")
comDFavail <- bind_rows(StillToDownload, repAvail)

### Update firstNR, and save:
if (firstNR > nrow(StillToDownload)) {
  firstNR <- 1
  allDownloaded <- TRUE
}

# if all the errors are of type "Unsupported get request", we can get rid of the errorLogR
errors <- bind_rows(errorLogR)
if (all(str_detect(errors$Msg, "Unsupported get request"))) errorLogR <- list()


save_Rep(comDFavail, repDF, errorLogR, allDone = allDownloaded, FN = useFN)


# If no errors occurred, clear the Temp Directory
if (is_empty(errorLogR)) clean_Directory("Data/TempR")

}


