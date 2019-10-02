
# DATE: 20.03.2019
# Facebook Politician Post collection


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

source("./R Code/FBCode/FaceBookUtilities.R")


set.seed(as.integer(Sys.time()))

#########################################################################################
###################### LOAD List of Politicians #########################################
#########################################################################################

# On April 19: 269 politicians with public FB accounts
polDF <- get_AllPolDF("FB")
polList <- polDF$polHandles


#########################################################################################
################## DOWNLOADING the Politican Timelines ##################################
#########################################################################################

limitDate <- lastPostDay # The last date on which to collect politician posts
N <- nrow(polDF)

d <- dir("Data/FBData/Tokens")
T <- length(d) # Number of tokens available


# get the status of the politician feed collection so far (firstNum, sinceDate, untilDate, errorLog)
list2env(get_PolFeed(returnRes = "Status"), envir = environment())

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
# useTokens <- c(useTokens, useTokens)

numComs <- N - firstNum + 1
perRound <- ceiling(numComs/T)
# perRound <- ceiling(numComs/(2*T))

untilDate <- min(max(untilDate, Sys.Date() - 2), limitDate)

FBFeedList <- vector("list", N)
names(FBFeedList) <- polDF$listName


sink(file = "Data/FBData/Temp/logfile.txt", split = TRUE)  # save to a logfile

print(Sys.time())
print("***************************************")
print("")

for (k in useTokens1) {
  # Decide which FB Token to use ... (1, 2, 3, 4, or 5)
  FBToken <- load_Token(k)
  
  print("")
  print(paste0("Loaded token number ", k))
  print("----------------------------")
  print("")
  
  # # or using GraphAPI (lasts only 2 hours)
  # FBToken <- "EAADVcRYENnkBAC88wsdxW7mVjHzyCNBTOEc16gGyQvWZAt4m3pQwwp4HyXvSzXUcTBwYVydk3dhitJzIRhBXeoFZAjlEJUZAocpLxMaZCZA0GcNMf5B4t9KZCkXnj0qasZCZCuNy2kA86RBnrslzL3Q8EZArs4aqH17t6rn8OQrNgq0s42G9nY7EPxQIgf9RXCj0ZD"
  # print("")
  # print("Using Graph API")
  # print("----------------------------")
  # print("")
  
  # count the number of posts collected so far
  numRounds <- 1
  
  # interimN: do we want to go all the way up to N, or should we stop before?
  interimN <- firstNum + perRound - 1
  
  startI <- firstNum
  endI <- min(N, interimN)
  
  
  FBFeedList[startI:endI] <- llply(polList[startI:endI], function(p) {
    print(p)
    
    resp <- tryCatch(get_FBPosts(p,
                                 sinceDate = sinceDate,
                                 untilDate = untilDate,
                                 num = 300,
                                 FBToken = FBToken),
                     error = function(e) {
                       print(e)
                       msg <- as.character(simpleError(e))
                       errorInfo <- list(Pol = p, Msg = msg, Time = Sys.time())
                       errorLog[[p]] <<- errorInfo
                       Sys.sleep(60)
                       return(NULL)
                     })
    
    # create a time stamp for saving a temporary file
    tStamp <- as.integer(Sys.time())
    save(resp, p, sinceDate, untilDate, file = paste0("Data/FBData/Temp/", tStamp, "_FBPosts.RData"))
    
    # Pause in-between rounds
    if (numRounds > 10) {
      numRounds <<- 1
      Sys.sleep(900) # 15 minutes
    } else {
      numRounds <<- numRounds + 1
      Sys.sleep(10) # 10 seconds
    }
    
    return(resp)
  })
  
  
  # update the status of politician comment collection:
  if (endI < N) {
    firstNum <- endI + 1 
  } else {
    firstNum <- 1
    sinceDate <- untilDate
    untilDate <- untilDate + 2
  }
  
  # save the results
  timeStamp <- as.integer(Sys.time())
  
  fName <- paste0(timeStamp, "_FBPosts.RData")
  save(FBFeedList, errorLog, firstNum, sinceDate, untilDate, file = file.path("Backup", fName))
  
  if (k < T) {
    print("")
    print("waiting between tokens")
    print("")
    Sys.sleep(900) # wait 15 minutes
  }
}

print("")
print("***************************************")
print("")
print("Done!")
print(Sys.time())

sink()

cleanedFBPosts <- addTo_PolFeed(PostList = FBFeedList, FBFeedFile = "CleanedPolFeeds.RData", minFileNum = 1L, errorLog, firstNum, sinceDate, untilDate)

## If the data collection was successful, clear out the Temp directory
if (is_empty(errorLog)) clean_Directory(dName = file.path("Data", "FBData", "Temp"))


