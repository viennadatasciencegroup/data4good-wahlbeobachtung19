
# DATE: 20.03.2019
# Facebook comment collection
# Automating the process, using a politicians dataset.
# Testing out the process under FB's new API Guidelines


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



set.seed(as.integer(Sys.time()))

#########################################################################################
####################  Get the Comments downloaded so far  ###############################
#########################################################################################

# Load the politicians
polDF <- get_AllPolDF("FB")

# First, get the df with politician posts, in Data/CleanedPolFeeds.RData
FBPosts <- get_PolFeed(returnRes = "Feed")




#########################################################################################
########################  Sort the Politician Posts  ####################################
#########################################################################################

# All Posts with comments up to max date are uptodate, 
# and don't need comments to be downloaded yet
# Also: get comments from posts that are 10 days or older
startDate <- firstDay
limitDate <- lastPostDay + 1
maxDate <- min(Sys.Date() - 1, lastDay)
minDate <- Sys.Date() - 5 - 2

useFN <- get_fileNum(sDir = "Data/FBData", fPattern = "FBComs", nPattern = "\\d+", decr = FALSE)
if (useFN == 0L) useFN <- get_fileNum(sDir = "Data/FBData/ready4Sampling", fPattern = "FBComs", nPattern = "\\d+", decr = TRUE) + 1

maxFileNum <- as.integer(minDate - startDate) %/% 3 + 1

if (useFN < maxFileNum) { # then we can do some comment collection:
  
  list2env(get_DownloadedComments(FN = useFN), envir = environment()) 
  
  # split the df into ones with comments already downloaded, and ones not:
  comAvail <- filter(FBPosts, FileNum != useFN | CommentsAvail >= as.Date(dateCreated) + 11)
  
  ComToDownload <- filter(FBPosts, FileNum == useFN, CommentsAvail < as.Date(dateCreated) + 11) 
  
  noComments <- filter(ComToDownload, comments_count == 0) %>%
    mutate(CommentsAvail = as.Date("2019-12-31"), RepAvail = as.Date("2019-12-31"))
  
  comAvail <- bind_rows(comAvail, noComments)
  
  StillToDownload <- filter(ComToDownload, comments_count > 0)
  
  allDownloaded <- FALSE
  
  
  #########################################################################################
  #########################  Download new comments ########################################
  #########################################################################################
  
  # what is the max number of comments to download per post??
  maxNum <- 24950  # FB does not allow more!!
  
  # To get All:
  maxComs <- nrow(StillToDownload)
  firstN <- 1
  numComs <- maxComs
  
  # Or get some at a time:
  firstN <- 2432
  maxComs <- nrow(StillToDownload)
  numComs <- maxComs - firstN + 1
  
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
  
  # add the new comments
  newComs <- NULL
  totalComments <- 0
  
  Sys.sleep(900)
  sink(file = "Data/FBData/TempC/logfile.txt", split = TRUE)
  for (k in useTokens) {
    # Decide which FB Token to use ...
    print("**********************************")
    print(paste0("Starting token ", k))
    print("")
    FBToken <- load_Token(k)
    
    startI <- firstN
    endI <- min(maxComs, firstN + perRound - 1)
    
    # # or using GraphAPI (lasts only 2 hours)
    # # FBToken <- "<fill in FB token here>"
    # FBToken <- "EAADVcRYENnkBAC88wsdxW7mVjHzyCNBTOEc16gGyQvWZAt4m3pQwwp4HyXvSzXUcTBwYVydk3dhitJzIRhBXeoFZAjlEJUZAocpLxMaZCZA0GcNMf5B4t9KZCkXnj0qasZCZCuNy2kA86RBnrslzL3Q8EZArs4aqH17t6rn8OQrNgq0s42G9nY7EPxQIgf9RXCj0ZD"
    
    for (s in startI:endI) {

      pol <- StillToDownload$Politician[s]
      replyTo <- StillToDownload$user[s]
      Lvl <- StillToDownload$Level[s]
      print(paste0("inside all_fb_comments, Politician, s is: ", pol, ", ", s))
      numComments <- StillToDownload$comments_count[s]
      totalComments <- totalComments + numComments
      
      if (numComments > 0) {
        untilDay <- min(as.Date(StillToDownload$dateCreated[s]) + 7, maxDate)
        res <- tryCatch(get_FBComments(StillToDownload$id[s], 
                                       sinceDate = StillToDownload$CommentsAvail[s],
                                       untilDate = untilDay,
                                       number = min(numComments + 10, maxNum),
                                       politician = pol, FBToken), 
                        error = function(e) {
                          print(e)
                          msg <- as.character(simpleError(e))
                          errID <- StillToDownload$id[s]
                          errorInfo <- list(Pol = pol, id = errID, Msg = msg, Time = Sys.time())
                          errorLogC[[errID]] <- errorInfo
                          Sys.sleep(900)
                          return(NULL)
                        })
        
        if (!(is_empty(res) || nrow(res) == 0)) {
          res$Politician <- pol
          res$replyToUser <- replyTo
          res$Level <- Lvl + 1L
          newComs <- bind_FBdfs(newComs, res)
          StillToDownload$CommentsAvail[s] <- Sys.Date() - 2
        }
        
        save(newComs, s, file = "Data/FBData/TempC/Comments.RData")
        if (totalComments %% 10000 == 0L) Sys.sleep(1200) else if (s %% 100 == 0L) Sys.sleep(300) else Sys.sleep(5)
      }
      
      
    }
    # newComs <- filter(newComs, !is.na(message), !(message %in% c("", " ")))  # don't get rid of empty messages -- might indicate deleted ones
    firstN <- endI + 1
    
    if (firstN > maxComs) break
    
    if (k != useTokens[T]) {
      print("Pause 10 mins before next token")
      Sys.sleep(600)
    }
  }
  
  sink()
  
  #########################################################################################
  ###################  Combine old & new comments, and save  ##############################
  #########################################################################################  
  
  
  #### DON'T HASH THE COMMENT ID's!!!!! Wait until after sampling! #####
  comDF <- newComs %>%
    dplyr::rename(user = from_name, created = created_time, text = message) %>%
#    filter(!is.na(text), !(str_squish(text) %in% c("", " "))) %>%
    clean_FBCommentsDF(polDF = polDF, morePols = morePols, Level = NULL) %>%
    bind_FBdfs(dfOld = comDF) %>%
    unique()
  
  list2env(get_PolFeed(returnRes = "Status"), envir = environment())
  
  cleanedFBPosts <- bind_rows(StillToDownload, comAvail)
  
  cleanedFBPosts <- mutate(cleanedFBPosts, Deleted = ifelse(id %in% deletedPosts, TRUE, 
                                                            ifelse(is.na(Deleted), FALSE, Deleted)))
  
  save_PolFeed(cleanedFBPosts, fName = "CleanedPolFeeds.RData", firstNum, sinceDate, untilDate, errorLog)
  
  
  
  ### Update firstNum, minDate, and save:
  if (firstN > nrow(StillToDownload)) {
    firstN <- 1
    allDownloaded <- TRUE
  }
  
  save_Com(comDF, errorLogC, FN = useFN, allDone = allDownloaded)
  
  comDFavail <- select(comDF, Politician, user, id, origPost, replyToID, dateCreated, comments_count, Level) %>%
    mutate(repAvail = as.Date(dateCreated))
  
  update_ComAvail(comDFavail, FN = useFN)
  
  # If no errors occurred, clear the Temp Directory
  if (is_empty(errorLogC)) clean_Directory("Data/FBData/TempC")
  
}

