

# DATE: 06.05.2019



library(plyr)
library(tidyverse)
library(stringr)
library(stringi) 
library(lubridate)
library(openssl)

# Sampling Facebook Comments





# Which variables should the final dataframe have?
# The dataframe columns are:
# - user (who posted the comment: as of GDPR, this should be anonymous unless user = Politician)
# - text (the text of the comment)
# - id (the message id: hashed, unless Politician comment)
# - replyToID (ID of the message being replied to, hashed)
# - Rating (Default value: -10; integer: -10 unrated, -5 not able to rate, 0 not offensive, 1, 2, 3, etc ... integers?? or characters??)
# - isPersonal (Default value should be "None")
# - Category (Default value should be "None")
# - Topic (Default value should be "None")
# - Level (0 = politician's post, 1 = reply to pol, 2 = reply to level 1, 3 = reply to level 2)
# - origPost (id of the original politician post being replied to)
# - Site (FaceBook or Twitter)
# - Include ("Y", or "N" - for messages that should not be evaluated, but are in the list as replyTo messages)
# - isFake (TRUE/FALSE)
# And the dataframes are in a named list: the list names are the Politician listNames from polDF


#########################################################################################
##########  Sample Comments and Prepare Packages of 100 Comments Each ###################
#########################################################################################

source("R Code/TwitterCode/TwitterUtilityFuncs.R")
source("R Code/FBCode/FaceBookUtilityFuncs.R")

sampleNum <- get_fileNum(fPattern = "SampledComments", nPattern = "\\d?[02468]", decr = TRUE) # catch 2-digit odd-numbered files

if (is_empty(sampleNum) || sampleNum == 0L) {
  sampleNum <- get_fileNum(sDir = "Data/forSampling/Packaged", 
                           fPattern = "SampledComments\\d?[02468].RData", 
                           nPattern = "\\d?[02468]", decr = TRUE)
}

sNum <- if (sampleNum == 0) 6 else sampleNum + 2  # account for the first sampling done previously and not using this numbering


polDF <- NULL
sampleRound <- sNum
maxPols <- 45


sample_Comments <- function(polDF = NULL, sampleRound = 1, maxPols = NULL){
  
  # create a directory "Packaged" where to archive FBComs & FBReps when done here!
  # and get rid of "empty" text
  useFN <- get_fileNum(sDir = "Data/FBData/ready4Sampling", fPattern = "FBReps", nPattern = "\\d+", decr = FALSE)
  comDF <- get_DownloadedComments(comFile = "ready4Sampling/FBComs", FN = useFN, comOnly = TRUE) %>%
    filter(!is.na(text), !(str_squish(text) %in% c("", " ")))
  repDF <- get_DownloadedReplies(repFile = "ready4Sampling/FBReps", FN = useFN, repOnly = TRUE) %>%
    filter(!is.na(text), !(str_squish(text) %in% c("", " "))) %>%
    filter(text != "[LEVEL 1 COMMENT AND REPLIES REMOVED]")
  
  userComments <- bind_FBdfs(comDF, repDF, checkAvail = FALSE, checkSampled = FALSE)
  
  polPosts <- get_PolFeed(returnRes = "Feed")
  
  
  if (is.null(polDF)) polDF <- get_AllPolDF("FB")
  
  polNames <- unique(userComments$Politician)
  
  samplePols <- update_usedPols(pNames = polNames, maxPols = maxPols) %>%
    intersect(polDF$listName)
  
  numPols <- length(samplePols)
  
  samplePolsDF <- filter(userComments, Politician %in% samplePols)

  otherPolsDF <- filter(userComments, !(Politician %in% samplePols))
  
  
  numCom <- get_NumCom()  # These are already in their listName form
  all(samplePols %in% names(numCom))
  
  samplePolsList <- split(samplePolsDF, samplePolsDF$Politician) %>%
    removeNull(isDF = TRUE)
  
  newPolsList <- vector("list", numPols)
  names(newPolsList) <- samplePols
  
  sampledComments <- llply(1:numPols, function(i) {
    currentPol <- samplePols[i]
    print(currentPol)

    isCurrentPol <- polPosts$Politician == currentPol
    df <- filter(polPosts, isCurrentPol)
    
    if (is_empty(df) || nrow(df) == 0) return(NULL)
    numPosts <- nrow(df)
    
    usePolComments <- filter(df, Sampled %in% c("N", "P"), FileNum == useFN)
    usePols <- filter(df, Sampled == "N")
    
    polSampleDF <- samplePolsList[[currentPol]]
    
    if (is_empty(polSampleDF) || nrow(polSampleDF) == 0) {
      if (numPosts < 100) return(NULL)  # we don't want politicians with less than 100 comments and/or posts
      polPosts$Sampled[isCurrentPol] <<- "Y"   # send back only the politician posts, and set them to included
      return(usePols)
    }
    
    
    numComPerPost <- numCom[[currentPol]]  # this is number of comments to sample per thousand comments
    if (numComPerPost < 500) maxToSample <- 100L else maxToSample <- 300L
    
    toSampleDF <- filter(polSampleDF, Sampled == "N", origPost %in% usePolComments$id) %>%
      filter(!is.na(text), !(str_squish(text) %in% c("", " ")))
    
    restDF <- polSampleDF %>%
      filter(Sampled == "Y" | !(origPost %in% usePolComments$id) | is.na(text) | str_squish(text) %in% c("", " "))
    
    selectDF <- ddply(toSampleDF, "origPost", .fun = function(x) {
      set.seed(95)
      r <- min(nrow(x), maxToSample)
      print(r)
      t <- sample.int(r, r)
      newX <- x[t, ]
      numCom <- round(numComPerPost*r/1000, 0L) %>%
        as.integer() %>%
        max(4L, na.rm = TRUE)   # at any rate, get at least 4 comments when available
      m <- min(r, numCom) 
      newX[1:m, ]$Sampled <- "Y"
      return(newX)
    }) 
    
    useReplies <- filter(selectDF, Sampled == "Y") %>%
      mutate(Include = "Y")
    
    numReplies <- nrow(useReplies)
    
    # update newPolsList
    newPolsList[[currentPol]] <<- bind_rows(selectDF, restDF) %>%
      unique()
    
    useReplyID <- useReplies$replyToID
    addRows <- filter(newPolsList[[currentPol]], id %in% useReplyID, !(id %in% useReplies$id)) %>%
      mutate(Include = "N")  # don't want to re-evaluate comments that were already done before, so in this round they will have Include set to "N"
    
    useReplies <- bind_rows(useReplies, addRows)
    
    # Add in the politician posts.  Don't forget older posts whose replies might be in the sample now
    if (!is_empty(usePols) && nrow(usePols) > 0) {
      usePols <- mutate(usePols, Politician = currentPol, origPost = "Post", Include = "Y") %>%
        select(-CommentsAvail, -RepAvail, -FileNum)
    } else usePols <- NULL
    addPols <- filter(df, Sampled %in% c("P", "Y"), id %in% useReplyID) 
    if (!is_empty(addPols) && nrow(addPols) > 0) {
      addPols <- mutate(addPols, Politician = currentPol, origPost = "Post", Include = "N") %>%
        select(-CommentsAvail, -RepAvail, -FileNum)
    }
    
    usePols <- bind_rows(usePols, addPols)
    
    # We have now included all politician posts so far, so update their status in df
    polPosts$Sampled[polPosts$id %in% usePols$id] <<- "P"  # if we've used the Post
    polPosts$Sampled[polPosts$id %in% usePolComments$id] <<- "Y"  # if we've used the post and the comment
    
    allComments <- bind_rows(usePols, useReplies) 
  })
  
  names(sampledComments) <- samplePols
  
  
  # Get rid of NULL elements in the list
  temp <- removeNull(sampledComments, isDF = TRUE)
  
  sampledComments <- temp
  
  # save the intermediate results
  timeStamp <- as.integer(Sys.time())
  
  fName <- paste0("Backup/", timeStamp, "_SampledComments", sampleRound, ".RData")
  save(sampledComments, file = fName)
  save(sampledComments, file = paste0("Data/forSampling/SampledComments", sampleRound, ".RData"))
  
  list2env(get_PolFeed(returnRes = "Status"), envir = environment())
  cleanedFBPosts <- polPosts
  save_PolFeed(cleanedFBPosts, errorLog, firstNum , sinceDate, untilDate, fName = "CleanedPolFeeds.RData")
  
  sampledPols <- bind_rows(newPolsList, .id = NULL)
  allComsReps <- bind_rows(sampledPols, otherPolsDF)
  save_sampledComReps(sampledDF = allComsReps, comDF = comDF, repDF = repDF, FN = useFN)
  
  return(sampledComments)
}
 


countSample <- ldply(sampledComments, function(df) {
  if (is_empty(df)) return(0)
  nrow(filter(df, Include == "Y"))
}, .id = "listName") %>%
  dplyr::rename(Count = V1)


AllPolComments <- llply(sampledComments, function(df) {
  pol <- unique(df$Politician)
  polHandle <- get_PolHandle(pol, polDF = polDF, Site = "FB")
  df <- df %>%
    mutate(user = ifelse(Level == 0L, polHandle, user)) %>%
    mutate(replyToUser = ifelse(Level == 1L, polHandle, replyToUser)) %>%
    clean_FBCommentsDF(polDF = polDF, morePols = morePols, Level = NULL, op = "hash") %>%
    select(user, text,  id, replyToUser,
           replyToID, Level, origPost, Include) %>%
    mutate(Rating = "-10",
           isPersonal = "None",
           Category = "None",
           Topic = "None",
           Site = "FB",
           isFake = FALSE)
})

# quick check that all replyToIDs are contained in the files
for (i in 1:length(AllPolComments)) {
  temp <- AllPolComments[[i]]
  temp2 <- filter(temp, Include == "Y", Level > 0L)
  if (!all(temp2$replyToID %in% temp$id)) {
    print(i)
  }
}


# Each dataframe corresponds to the comments made to and by a Politician.
# The dataframe columns are:
# - user (who posted the comment: as of GDPR, this should be anonymous unless user = Politician)
# - text (the text of the comment)
# - id (the message id: hashed, unless Politician comment)
# - replyToUser (the message was posted in reply to a message by this user: again, anonymous unless reply to Politician)
# - replyToID (ID of the message being replied to, hashed)
# - Rating (Default value: -10; integer: -10 unrated, -5 not able to rate, 0 not offensive, 1, 2, 3, etc ... integers?? or characters??)
# - isPersonal (Default value should be "None")
# - Category (Default value should be "None")
# - Topic (Default value should be "None")
# - Level (0 = politician's post, 1 = reply to pol, 2 = reply to level 1, 3 = reply to level 2)
# - origPost (id of the original politician post being replied to)
# - Site (FaceBook or Twitter)
# - Include ("Y", or "N" - for messages that should not be evaluated, but are in the list as replyTo messages)
# - isFake True/False



Round <- sNum
numTimes <- 0
sampleNum <- sNum
save(AllPolComments, Round, numTimes, file = paste0("Data/forSampling/PolComments", sampleNum, ".RData"))

timeStamp <- as.integer(Sys.time())

fName <- paste0("Backup/", timeStamp, "_PolComments", sampleNum, ".RData")
save(AllPolComments, Round, numTimes, file = fName)

fName <- paste0("SampledComments", sampleNum, ".RData")
archive_samplingFiles(fName)
