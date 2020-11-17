
# DATE CREATED: 16.03.2019
# This file contains various utility functions for the collection of Tweets



#########################################################################################
####################  Anonymize User Comments ###########################################
#########################################################################################

# Remove all @ mentions of users not in the politicians list
make_anon <- function(t, polDF = NULL, otherPols = NULL) {
  if (is.null(polDF)) polDF <- get_AllPolDF()
  foundPols <- c(polDF$polHandles, otherPols)
  searchPols <- str_to_lower(foundPols)
  
  toPattern <- "@([\\w]*)"
#  toPattern <- "@([^\\s@]*)"
#  toPattern <- "(@([^\\s]*)(\\s)*)*"  used to extract several @'s in a row, but don't need this here
  
  # create a little function to replace generic user names with @Anonymer_User
  replace_ats <- function(s) {
    newS <- s %>%
      str_replace_all("@", "") %>%
      str_squish()
    
    result <- ifelse(str_to_lower(newS) %in% searchPols, paste0("@", newS), ifelse(stri_isempty(newS), "", "@Anonymer_User"))
    return(result)
  }
  
  # remove all the @ parts of comment
  newText <- str_replace_all(t, pattern = toPattern, replace_ats) 
}


# Hash the id in v if name is not one of the politician Twitter/FB Handles
hash_id <- function(v, name, polDF = NULL, otherPols = NULL) {
  if (is_empty(v) || all(is.na(v)) || all(v == "")) return(NA)
  if (is.null(polDF)) polDF <- get_AllPolDF()
  foundPols <- c(polDF$polHandles, otherPols)
  searchPols <- str_to_lower(foundPols)
  
  newV <- ifelse(str_to_lower(name) %in% searchPols, v, sha256(x = v, key = rnum))
  
  return(newV)
}

#########################################################################################
####################  Clean Tweet Data Frame ############################################
#########################################################################################

# This function will return a dataframe with cleaned up data retrieved from Twitter.
# Sensitive user names and ids will be removed - only politicians from the list will remain (this is what the term anonymized means)
# sensitive status IDs (the id of the tweet itself) will be hashed (only the politicians' from the list will remain unchanged)

# What to keep:
# user = screen_name  --> anonymized
# userID = user_id  --> hashed
# text = text  --> anonymized
# id = status_id --> hashed
# replyToUser = reply_to_screen_name --> anonymized
# replyToID = reply_to_status_id --> hashed
# dateCreated = created_at
# favoriteCount = favorite_count
# retweetCount = retweetCount
# verified
# isQuote = is_quote
# quoted_text  --> anonymized
# quoted_screen_name  --> anonymized
# quoted_user_id  --> hashed
# quoted_status_id  --> hashed
# quoted_favorite_count
# quoted_retweet_count
# quoted_verified
# isRetweet = is_retweet
# retweet_text  --> anonymized
# retweet_screen_name  --> anonymized
# retweet_user_id  --> hashed
# retweet_status_id  --> hashed
# retweet_favorite_count
# retweet_retweet_count
# retweet_verified

# op can be "all", "select", "hash", and/or "anon"
# for now, setting default to c("select", "hash")
# but in the future default should be "all"!!!
clean_TweetsDF <- function(df, polDF = NULL, morePols = NULL, op = "all"){
  
  if (is_empty(polDF)) polDF <- get_AllPolDF()
  
   searchPols <- union(polDF$polHandles, morePols) %>%
     str_to_lower() 
  
  newDF <- df
  
  # select the variables we want to keep
  if (any(op %in% c("all", "select"))) {
    newDF <- newDF %>%
      select(
        user = screen_name,
        userID = user_id,
        text,
        id = status_id,
        replyToUser = reply_to_screen_name,
        replyToID = reply_to_status_id,
        dateCreated = created_at,
        favoriteCount = favorite_count,
        retweetCount = retweet_count,
        verified,
        isQuote = is_quote,
        quoted_text,
        quoted_screen_name,
        quoted_status_id,
        quoted_favorite_count,
        quoted_retweet_count,
        quoted_verified,
        isRetweet = is_retweet,
        retweet_text,
        retweet_screen_name,
        retweet_status_id,
        retweet_favorite_count,
        retweet_retweet_count,
        retweet_verified,
        source
      ) %>%
      mutate(Sampled = "N")  # Add this, so we know whether a comment has already been sampled or not
  }
  
  # hash IDs
  if (any(op %in% c("all", "hash"))) {
    newDF <- newDF %>%
      mutate(
        id = hash_id(id, user, polDF = polDF, otherPols = morePols),
        replyToID = hash_id(replyToID, replyToUser, polDF = polDF, otherPols = morePols),
        quoted_status_id = hash_id(quoted_status_id, quoted_screen_name, polDF = polDF, otherPols = morePols),
        retweet_status_id = hash_id(retweet_status_id, retweet_screen_name, polDF = polDF, otherPols = morePols)
      ) 
  }
  
  # Anonymize generic user names
  if (any(op %in% c("all", "anon"))) {
    newDF <- newDF %>%
      mutate(
        text = make_anon(text, polDF = polDF, otherPols = morePols),
        quoted_text = make_anon(quoted_text, polDF = polDF, otherPols = morePols),
        retweet_text = make_anon(retweet_text, polDF = polDF, otherPols = morePols)
      ) %>%
      mutate(
        user = ifelse(str_to_lower(user) %in% searchPols, user, "Anonymous"),
        userID = ifelse(str_to_lower(user) %in% searchPols, userID, NA),
        replyToUser = ifelse(str_to_lower(replyToUser) %in% searchPols, replyToUser, "Anonymous"),
        quoted_screen_name = ifelse(str_to_lower(quoted_screen_name) %in% searchPols, quoted_screen_name, "Anonymous"),
        retweet_screen_name = ifelse(str_to_lower(retweet_screen_name) %in% searchPols, retweet_screen_name, "Anonymous")
      ) 
  }
  
  return(unique(newDF))
}


#########################################################################################
#########  Get the most recent politician timelines, and join to existing file ##########
#########################################################################################

update_minComID <- function(currMinComID) {
  
  fName <- "CleanedPolTimeLines.RData"
  fPath <- file.path("Data", "TwData", fName)
  
  if (file.exists(fPath)) {
    
    load(fPath)
    
    nextMinComID <- currMinComID
    tStamp <- as.integer(Sys.time())
    
    # save the latest cleaned file
    save(nextMinID, nextMinComID, lastDate, cleanPolTL, file = paste0("Data/TwData/", fName))  
    save(nextMinID, nextMinComID, lastDate, cleanPolTL, file = paste0("Backup/", tStamp, "_", fName)) 
    
    return(TRUE)
  }
  return(FALSE)
}



# Get the file with politician posts
get_PolTimeline <- function(dfOnly = TRUE) {
  
  fName <- "CleanedPolTimeLines.RData"
  fPath1 <- file.path("Data", "TwData", fName)
  fPath2 <- file.path("..", "Collect Comments", fPath1)
  
  
  if (file.exists(fPath1)) {
    fPath <- fPath1 
  } else if (file.exists(fPath2)) {
    fPath <- fPath2
  } else fPath <- NULL
  
  if (!is_empty(fPath)) {
    
    attach(fPath)
    
    PolTL <- cleanPolTL
    maxLen <- max(nchar(cleanPolTL$id), 0, na.rm = TRUE)
    lowestID <- nextMinID
    lowestComID <- nextMinComID
    lastDate <- lastDate
    
    detach(paste0("file:", fPath), character.only = TRUE)
  } else {
    PolTL <- NULL
    maxLen <- 0L
    lowestID <- "0"
    lowestComID <- "0"
    lastDate <- "1990-09-09"
  }
  
  if (dfOnly == TRUE) {
    return(PolTL)
  } 
  return(list(PolTL = PolTL, maxLen = maxLen, lowestID = lowestID, lowestComID = lowestComID, lastDate = lastDate))
}


# download the politician timelines for the handles available in polDF
# download the tweets up to and including the date given by useDate 
# start the tweets at firstDay (defined in GeneralUtilityFuncs)
# since get_timelines uses tweet ID and not dates for bounding tweets,
# the first time we run the code, we need to determine the minimum tweet ID allowed
# this is usually given by minimumID, but on the first run, we don't know what this ID should be
# by setting useMin = TRUE and minimumID = 0, we tell the function to determine this overall minimum ID
# and save it.

update_PolTimelines <- function(polDF = NULL, morePols = NULL, numPosts = 100, 
                                useMin = FALSE, minimumID = "0", useDate = NULL,
                                fName = "CleanedPolTimeLines") {
  
  
  if (is.null(useDate)) useDate <- min(Sys.Date() - 2, lastPostDay)
  if (is.null(polDF)) polDF <- get_AllPolDF()
  usePolHandles <- polDF$polHandles
  
  # Get the polTimelines obtained so far (if they exist):
  oldPolList <- get_PolTimeline(dfOnly = FALSE)
  oldPolTL <- oldPolList$PolTL
  useMinID <- oldPolList$lowestID
  nextMinComID <- oldPolList$lowestComID  # we're not going to change this when we update the pol timeline
  
  # if there is no old politician TimeLine, use the minID passed in to the function
  if (is_empty(oldPolTL) || useMin == TRUE) useMinID <- minimumID  
  
  # Get the new timelines
  t0 <- get_timeline(user = usePolHandles, n = numPosts, min_id = useMinID, token = TwToken)
  
  # Keep track of politicians whose comments were not found:
  foundPols <- unique(t0$screen_name)
  notFound <- setdiff(str_to_lower(usePolHandles), str_to_lower(foundPols))
  
  if (is_empty(t0) || nrow(t0) == 0L) return(notFound)
  
  # Keep only the new posts (not the ones previously downloaded)
  maxLen <- max(nchar(t0$status_id), nchar(useMinID), 0, na.rm = TRUE)
  useMaxID <- get_BoundaryID(useDate, polTL = t0, startID = "0", includeDF = FALSE)
  useMaxID <- useMaxID$maxID
  
  # if this is the first time we download posts, get the starting tweet ID and save it to file
  if (useMin == TRUE && minimumID == "0") {
    useID <- get_BoundaryID(firstDay, polTL = t0, startID = "0", includeDF = FALSE)
    useMinID <- useID$minID
    saveRDS(useMinID, "PoliticianList/TwStartID.RDS")
  }
  
  useMinID <- pad_id(useMinID, maxLen)
  useMaxID <- pad_id(useMaxID, maxLen)
  
  
  
  print(paste0("In update pol timeline, maxID is: ", useMaxID))
  
  save(t0, useMinID, useMaxID, useDate, file = "Data/TwData/TwLogs/tempTL.RData")
  
  t1 <- mutate(t0, padded_status_id = pad_id(status_id, maxLen)) %>%
    filter(padded_status_id > useMinID, padded_status_id < useMaxID)
  
  if (is_empty(t1) || nrow(t1) == 0L) return(notFound)
  
  polTimelines <- clean_TweetsDF(df = t1, polDF = polDF, morePols = morePols)
  
  pTL1 <- filter(polTimelines, str_to_lower(user) %in% str_to_lower(polDF$polHandles))
  pTL2 <- filter(polTimelines, !(str_to_lower(user) %in% str_to_lower(polDF$polHandles))) %>%
    mutate(Politician = user)
  
  if (!is_empty(pTL1) && nrow(pTL1) > 0L) {
    pTL1 <- mutate(pTL1,
                   Politician = ldply(str_to_lower(user), get_listName, Site = "Twitter", polDF = polDF)$V1)
  }
  
  polTimelines <- bind_rows(pTL1, pTL2)
  
  # keep track of missing politicians for future retrials:
  if (!is_empty(notFound)) {
    save(notFound, useMinID, useMaxID, file = paste0("Data/TwData/TwLogs/", as.integer(Sys.time()), "_missingPols.RData"))
  }
  
  # Create the new pol timelines by binding the old ones to the new  
  cleanPolTL <- bind_dfs(dfOld = oldPolTL, dfNew = polTimelines, checkSampled = TRUE)
  lastID <- cleanPolTL %>%
    mutate(padID = pad_id(id, maxLen)) %>%
    arrange(desc(padID))
  
  lastIndex <- min(nrow(lastID), 5)
  nextMinID <- lastID$id[lastIndex]
  
  lastDate <- useDate
  tStamp <- as.integer(Sys.time())
  
  # save the latest cleaned file
  save(nextMinID, nextMinComID, lastDate, cleanPolTL, file = paste0("Data/TwData/", fName, ".RData"))  
  save(nextMinID, nextMinComID, lastDate, cleanPolTL, file = paste0("Backup/", tStamp, "_", fName, ".RData")) 
  
  # return the vector of politicians whose timelines were not found
  return(notFound)
}


#########################################################################################
###########  Get the original politician post to a user comment at any level ############
#########################################################################################

# Given x, a vector of replyToIDs, and df, the data frame with all comments and posts,
# this function returns a vector with the tweet id of the original politician post that the
# given comment is in the thread of

get_origPost <- function(x, df) {
  result <- ldply(x, function(y) {
    res <- filter(df, id == y)
    return(res$origPost[1])
  })[[1]]
  return(result)
}


#########################################################################################
######################  Getting Comments ################################################
#########################################################################################



get_PolLevels <- function(fName = "pol4Levels.RData", numPols = 3, listN = NULL) {
  fPath1 <- file.path("Data", "TwData", fName)
  fPath2 <- file.path("..", "Collect Comments", fPath1)
  
  if (file.exists(fPath1)) {
    fPath <- fPath1 
  } else if (file.exists(fPath2)) {
    fPath <- fPath2
  } else fPath <- NULL
  
  if (!is_empty(fPath)) {
    attach(fPath)
    newPolLevels <- newPolLevels
    
    detach(paste0("file:", fPath), character.only = TRUE)
  } else {
    newPolLevels <- vector("list", numPols)
    names(newPolLevels) <- listN
  }
  
  return(newPolLevels)
}


get_atComments <- function(fName = "CleanedtoComments.RData") {
  
  fPath <- file.path("Data", "TwData", fName)
  if (file.exists(fPath)) {
    
    attach(fPath)
    envName <- paste0("file:", fPath)
    inFile <- ls(envName)
    toCom <- get(inFile, pos = envName)
    
    detach(envName, character.only = TRUE)
  } else {
    toCom <- NULL
  }
  return(toCom)
  
}

# Retrieve all twitter comments @politicians in the sTerms vector,
# (or, if sTerms is null, all politician handles in polDF)
# whose Twitter id lies between lowerB and upperB.
# lowerB is obtained by looking at the last id from the day prior to sinceDay, while
# upperB is obtained by looking at the minimum id from the day after untilDay.
# A default minID is included (this one corresponding to September 8, 2019)
# save comments, and return them
download_atComments <- function(polDF = NULL, polTml = NULL, sTerms = NULL, lNames = NULL,
                                sinceDay, untilDay = NULL, numComments = 1000, morePols = NULL, 
                                minID = NULL, useAPI = "standard",
                                product = NULL, twEnv = NULL, ...) {
  
  # Figure out what the sTerms are (i.e. which politicians to search for)
  if (is.null(polDF)) {
    polDF <- get_AllPolDF()
  } 
  
  if (is.null(sTerms)) {
    Users <- str_to_lower(polDF$polHandles)
    sTerms <- paste0("@", Users)
  }
  
  if (is.null(lNames)) {
    lNames <- polDF$listName
  }
  
  if (is.null(minID)) {
    if (file.exists("PoliticianList/TwStartID.RDS")) {
      minID <- readRDS("PoliticianList/TwStartID.RDS")
    } else minID <- "0"
  }
  
  # Get the politician Timelines (politician comments downloaded so far)
  polTmlList <- get_PolTimeline(dfOnly = FALSE)
  currMinComID <- polTmlList$lowestComID
  
  if (is.null(polTml)) {
    lastDate <- polTmlList$lastDate
    currDate <- max(Sys.Date() - 3, untilDay) %>%
      min(lastPostDay)
    
    if (lastDate < currDate) {   # If the politician timelines are not recent enough, update them
      
      update_PolTimelines(polDF = polDF, morePols = morePols, numPosts = 100, minimumID = minID)
      
      print("Taking a break before downloading comments")
      Sys.sleep(1800) # Take a 30 min break before continuing with comment download
      
      polTml <- get_PolTimeline()
    } else {
      polTml <- polTmlList$PolTL
    }
  } 
  
  rm(polTmlList)
  
  # If using Standard API, use the polTL to determine the lowerB and upperB related to sinceDay and untilDay
  if (useAPI == "standard") {
    newList <- get_BoundaryID(polTL = polTml, useDate = sinceDay, includeDF = FALSE)
    lowerB <- newList$minID
    if (is_empty(lowerB) || is.na(lowerB)) lowerB <- currMinComID 
    
    if (is_empty(untilDay)) {
      untilDay <- sinceDay
      upperB <- newList$maxID  
    } else {
      newList <- get_BoundaryID(polTL = polTml, useDate = untilDay - 1)
      upperB <- newList$maxID
    }

      idLen <- max(nchar(lowerB), nchar(upperB))
      upperB <- pad_id(upperB, idLen)
      lowerB <- pad_id(lowerB, idLen)
  
    
    lowerB <- max(lowerB, pad_id(minID, idLen), na.rm = TRUE) # at any rate, we don't want comments from before Sept. 8th
    
  } else {
    if (is_empty(untilDay)) untilDay <- Sys.Date() - 3
    newList <- get_BoundaryID(polTL = polTml, useDate = untilDay, includeDF = FALSE)
    upperB <- newList$maxID
    print(upperB)
    idLen <- max(nchar(minID), nchar(upperB))
    print(idLen)
    lowerB <- pad_id(minID, idLen)
  }
  
  
  # Keep track of the total number of comments downlaoded in the current 15-min window,
  # to avoid rate limiting.  When we get close to 180000, pause for 15 mins
  numDownloaded <- 0
  nextMinComID <- NULL
  
  print(lowerB)
  print(upperB)
  # Download comments directed at sTerms
  atComments <- llply(sTerms, function(q) {
    TwError <- TRUE
    df <- NULL
    
    remainingComments <- numComments # how many comments do we still need
    while (TwError == TRUE) {   # Keep trying to download for this particular searchTerm
      TwError <- FALSE
      print(q)
      newDF <- tryCatch(search_tweets(q,  
                                      include_rts = TRUE, 
                                      n = remainingComments, 
                                      max_id = upperB,
                                      min_id = lowerB,
                                      retryonratelimit = TRUE,
                                      useAPI = useAPI,
                                      product = product,
                                      twEnv = twEnv,
                                      token = TwToken,
                                      ...), 
                        error = function(e) {
                          print("waiting")
                          Sys.sleep(300)
                          TwError <<- TRUE
                        })
      
      
      # break if the newDF is empty because there are no comments (and not due to error)
      emptyDF <- (is_empty(newDF) || nrow(newDF) == 0 || is_empty(newDF$status_id))
      if (emptyDF && TwError == FALSE) break
      
      if (!emptyDF) {
        
        if (is_empty(upperB)) {
          upperBound <- get_BoundaryID(untilDay, polTL = newDF, startID = "0", includeDF = FALSE)
          upperB <- upperBound$maxID
        }
        
        maxLen <- max(nchar(newDF$status_id), idLen, 0, na.rm = TRUE)
        dfIDs <- pad_id(newDF$status_id, maxLen)
        if (maxLen > idLen) {
          upperB <- pad_id(upperB, maxLen)
          lowerB <- pad_id(lowerB, maxLen)
        }
        
        # if in this iteration we still haven't gone all the way to the lowerB ID
        # we set TwError to TRUE (so that the loop will continue) and
        # set upperB to the minimum of the ID's we have found so far
        if ((min(dfIDs, upperB, na.rm = TRUE) > lowerB) && (useAPI == "standard")) {
          TwError <- TRUE
          upperB <- min(dfIDs, na.rm = TRUE)
        }
        
        
        df <- bind_rows(df, newDF) %>%
          unique()
        
        remainingComments <- numComments - nrow(df)
        
        if (remainingComments < 1) TwError <- FALSE  # stop the while loop when we have found enough comments
        
        numDownloaded <- numDownloaded + nrow(df)
        if (numDownloaded > 17500) {
          print("wait or else we'll exceed rate-limit")
          Sys.sleep(960) # sleep for 16 minutes
          numDownloaded <- 0
        }
      }
      
      if (useAPI == "premium") {
        print("Resting a few seconds")
        Sys.sleep(4)
      }
    }
    
    return(df)
  })
  
  ## save the result temporarily
  names(atComments) <- lNames
  
  #create timeStamp
  t <- as.integer(Sys.time())
  
  fName <- paste0("Backup/", t, "_polReplies.RData")
  save(atComments, file = fName)
  
  
  
  # compute the next minimum comment ID, which we can use as lower bound for the next comments to collect
  # We do this by looking at the maximum comment ID collected for each politician so far
  # and concatenating the current minimum comment id
  nextMinCom <- ldply(atComments, function(df) {
    if (is_empty(df) || nrow(df) == 0L) return(NULL)
    
    currDayDF <- filter(df, as.Date(created_at) < untilDay + 1) 
    if (is_empty(currDayDF) || nrow(currDayDF) == 0L) return(NULL)
    
    idVec <- currDayDF$status_id
    idLen <- max(nchar(idVec))
    idVec <- pad_id(idVec, idLen)
    nextMinComID <- max(idVec, na.rm = TRUE)
    return(nextMinComID)
  })$V1 %>%
    c(currMinComID)
  
  idLen <- max(nchar(nextMinCom))
  maxIndex <- min(3, length(nextMinCom) - 1)
  
  # to create a bit of overlap, we'll use the 3rd largest comment ID currently collected as the minimum for the
  # next round
  nextMinComID <- pad_id(nextMinCom, idLen) %>%
    sort(decreasing = TRUE) %>%
    .[maxIndex]
  
  update_minComID(nextMinComID)
  
  return(atComments)
}


# Get new at_comments for startDay, ending at stopDay.
# Default for startDay: 4 days ago.
# Add the new at_comments to the file with previously collected and cleaned at_comments,
# contained in the file fName (default = "CleanedtoComments.RData")
# At the same time update the 4levels file
update_atComments <- function(polDF = NULL, polTml = NULL, startDay = NULL, stopDay = NULL,
                              sTerms = NULL, listNames = NULL, 
                              numComments = 100, morePols = NULL, 
                              fName = "CleanedtoComments.RData", newToCom = NULL, useAPI = "standard",
                              product = NULL, twEnv = NULL, ...) {
  
  if (is.null(polDF)) {
    polDF <- get_AllPolDF()
  } 
  
  if (is.null(startDay)) startDay <- Sys.Date() - 4
  
  # Get the politician Timelines (file with politician posts)
  if (is.null(polTml)) {
    polTmlList <- get_PolTimeline(dfOnly = FALSE)
    lastDate <- polTmlList$lastDate
    currDate <- max(Sys.Date() - 3, stopDay) %>%
      min(lastPostDay)
    
    if (lastDate < currDate) {   # If the politiican timelines are not recent enough, update them
      update_PolTimelines(polDF = polDF, morePols = morePols, numPosts = 100)
      polTml <- get_PolTimeline()
      
      print("Taking a break before downloading comments")
      Sys.sleep(1800) # Take a 30 min break before continuing with comment download
      
    } else {
      polTml <- polTmlList$PolTL
    }
  } 
  
  # if the search terms were not included, derive them from polDF
  if (is.null(sTerms)) {
    Users <- str_to_lower(polDF$polHandles)
    sTerms <- paste0("@", Users)
  }
  
  # if the list names were not included, derive them from polDF
  if (is.null(listNames)) {
    listNames <- polDF$listName
  }
  
  # get the old atComments from file if the filename has been supplied
  oldToCom <- NULL
  if (!is_empty(fName) && !(fName == "")) {
    oldToCom <- get_atComments(fName)
  }
  
  # newToCom can be character (the filename to open)
  if (is.character(newToCom)) {
    newToCom <- get_atComments(newToCom)
  } 
  
  # or a list, or NULL.  When Null, download more comments ..
  if (is.null(newToCom)) {
    newToCom <- download_atComments(polDF = polDF, polTml = polTml,
                                    sTerms = sTerms, lNames = listNames, sinceDay = startDay, untilDay = stopDay,
                                    numComments = numComments, morePols = morePols, useAPI = useAPI,
                                    product = product, twEnv = twEnv, ...)
  }
  
  
  numIntersect <- rep(0, nrow(polDF))
  names(numIntersect) <- listNames
  
  # get the file with the comments by level made up to date
  newPolLevels <- get_PolLevels(numPols = nrow(polDF), listN = polDF$listName)
  
  # check that all comments are in pol4levels before starting
  for (l in listNames) {
    df1 <- oldToCom[[l]]
    df2 <- newPolLevels[[l]]
    if (!all(df1$id %in% df2$id)) {
      warning(paste0("Politician: ", l, ".  Missing comments before even starting with new ones."))
    }
  }
  
  toComments <- llply(listNames, function(i) {
    print(paste0("Now working on list name: ", i))
    df1 <- oldToCom[[i]]
    df2 <- newToCom[[i]]
    
    pHandle <- get_PolHandle(pName = i, polDF = polDF)
    isNullDF1 <- is_empty(df1) || nrow(df1) == 0
    isNullDF2 <- is_empty(df2) || nrow(df2) == 0
    
    if (!isNullDF2) {
      minID <- readRDS("PoliticianList/TwStartID.RDS")   # make sure we include only comments from Sept. 8 or later
      
      maxLen <- max(nchar(polTml$id), nchar(df2$status_id), nchar(minID), na.rm = TRUE) 
      minID <- pad_id(minID, maxLen)
      
      moreThanMin <- pad_id(df2$status_id, maxLen) > minID
      
      df2 <- df2 %>%
        filter(moreThanMin) 
      
      newMin <- filter(df2, created_at < firstDay) %>%
        pull(status_id) %>%
        pad_id(maxLen) %>%
        max(minID, na.rm = TRUE)
      
      if (newMin > minID) saveRDS(newMin, "PoliticianList/TwStartID.RDS")
      
      moreThanMin <- pad_id(df2$status_id, maxLen) > newMin
      
      df2 <- df2 %>%
        filter(moreThanMin) %>%
        clean_TweetsDF(polDF = polDF, morePols = morePols)
      
      tempDF <- make_4Levels(allTweets = df2, 
                             polHandle = pHandle, 
                             polTml = polTml,
                             oldPolLvl = newPolLevels[[i]], 
                             polDF = polDF)
      
      
      ### Checks before exiting ###-------------------------------------------
      
      
      if (!all(df1$id %in% tempDF$id)) {
        msg <- paste0("Politician: ", i, ".  Missing old comments in newPolLevels")
        warning(msg)
      }
      
      if (!all(df2$id %in% tempDF$id)) {
        msg <- paste0("Politician: ", i, ".  Missing new comments in newPolLevels.")
        warning(msg)
      }
      
      newPolLevels[[i]] <<- tempDF
      rm(tempDF)
    }
    
    
    
    
    if (isNullDF1 || isNullDF2) numIntersect[i] <<- 0 else numIntersect[i] <<- nrow(intersect_dfs(df1, df2))
    
    newDF <- bind_dfs(df1, df2, onePol = TRUE)
    
    return(newDF)
  })
  
  
  names(toComments) <- listNames
  
  # Using lubridate to create dates of the form "ddmmyyyy"
  f <- stamp_date("31092018")
  
  if (is.null(fName)) fName <- "CleanedtoComments.RData"
  save(toComments, file = file.path("Data", "TwData", fName))
  save(toComments, file = paste0("Backup/", f(Sys.Date()), "_", fName))
  
  # Are there any dfs without intersections (might be sign of error)?
  noIntersection <- which(numIntersect == 0)
  save(startDay, noIntersection, file = paste0("Data/TwData/TwLogs/", as.integer(Sys.time()), "_missingAtComments.RData"))
  
  
  # Using lubridate to create dates of the form "ddmmyyyy"  and save backup
  f <- stamp_date("31092018")
  backupPath <- paste0("Backup/", f(Sys.Date()), "_pol4Levels.RData")
  save(newPolLevels, file = backupPath)
  
  # get rid of NULLS in newPolLevels
  newPolLevels <- removeNull(newPolLevels, isDF = TRUE)
  
  ### save polLevels and backup
  fPath <- file.path("Data", "TwData", "pol4Levels.RData")
  save(newPolLevels, file = fPath)
  save(newPolLevels, file = backupPath)
  
  result <- list(polLvls = newPolLevels, noInt = noIntersection)
  return(result)
}


# Return 11 levels of twitter comments
# where a level 0 comment is the original politician post
# level 1 is a user comment directly replying to a politician post
# level 2 is a user comment replying to a level 1, etc...
# Also: level 10 is a user comment replying to the politician but not to a post 
# (i.e. politician is first mention, or listed in replyToUser)
# Level 11 is a reply to a Level 10, etc...
# Level 20 collects all user comments that mention the politician, but do not fall into any of the
# previous categories.
#
# allTweets is the list of polReplies obtained in get_atComments,
# and polTml is the politician Timeline data frame


make_4Levels <- function(allTweets, polHandle, oldPolLvl = NULL, polTml = NULL, polDF = NULL) {
  
  # Get the politician Timelines (file with politician posts)
  if (is.null(polTml)) polTml <- get_PolTimeline()
  
  # Get the file with politician names and twitter handles
  if (is.null(polDF)) polDF <- get_AllPolDF()
  
  # which Politician has the handle polHandle?
  polName <- get_listName(polHandle, polDF = polDF)
  
  # Get the post for the politician with polHandle Twitter handle
  TmlDF <- filter(polTml, Politician == polName)
  
  
  if (!is_empty(oldPolLvl)) {
    if (any(duplicated(oldPolLvl$id))) {
      warning(paste0("Politician: ", polName, ".  The old pol4Levels had duplicate IDs.  Removing them."))
      oldPolLvl <- arrange(oldPolLvl, id, replyLevel) %>%
        filter(!duplicated(id))
    } 
    
    # make the new comments by level
    oldDF <- filter(oldPolLvl, !(id %in% allTweets$id))
  } else oldDF <- NULL
  
  if (!is_empty(oldDF)) {
    oldDFL1 <- filter(oldDF, replyLevel == 1L)
    oldDFL2 <- filter(oldDF, replyLevel == 2L)
    oldDFL3 <- filter(oldDF, replyLevel == 3L)
    oldDFL4 <- filter(oldDF, replyLevel == 4L)
    
    oldDFL10 <- filter(oldDF, replyLevel == 10L, !(replyToID %in% polTml$id))
    allTweets <- filter(oldDF, replyLevel == 10L, replyToID %in% polTml$id) %>%
      bind_rows(allTweets)
    
    newIDs <- c(allTweets$id, polTml$id)
    oldDFL11 <- filter(oldDF, replyLevel == 11L, !(replyToID %in% newIDs))
    allTweets <- filter(oldDF, replyLevel == 11L, replyToID %in% newIDs) %>%
      bind_rows(allTweets)
    
    oldDFL12 <- filter(oldDF, replyLevel == 12L, !(replyToID %in% newIDs))
    allTweets <- filter(oldDF, replyLevel == 12L, replyToID %in% newIDs) %>%
      bind_rows(allTweets)
    
    oldDFL13 <- filter(oldDF, replyLevel == 13L, !(replyToID %in% newIDs))
    allTweets <- filter(oldDF, replyLevel == 13L, replyToID %in% newIDs) %>%
      bind_rows(allTweets)
    
    oldDFL14 <- filter(oldDF, replyLevel == 14L, !(replyToID %in% newIDs))
    allTweets <- filter(oldDF, replyLevel == 14L, replyToID %in% newIDs) %>%
      bind_rows(allTweets)
    
    oldDFL20 <- filter(oldDF, replyLevel == 20L, !(replyToID %in% newIDs)) 
    allTweets <- filter(oldDF, replyLevel == 20L, replyToID %in% newIDs) %>%
      bind_rows(allTweets) 
    
  } else {
    oldDFL1 <- oldDFL2 <- oldDFL3 <- oldDFL4 <- NULL
    oldDFL10 <- oldDFL11 <- oldDFL12 <- oldDFL13 <- oldDFL14 <- oldDFL20 <- NULL
  }
  
  # if there are no new tweets to assign to a Level, return the old polLevel:
  if (is_empty(allTweets) || nrow(allTweets) == 0L) return(oldPolLvl)
  
  # Tweets that mention the politician, but are not replies to comments
  newDFL20 <- filter(allTweets, is.na(replyToID))
  
  newDFL10 <- filter(newDFL20, str_to_lower(replyToUser) == str_to_lower(polHandle))
  newDFL20 <- filter(newDFL20, !(id %in% newDFL10$id))
  
  addDFL10 <- filter(newDFL20, str_detect(str_to_lower(newDFL20$text), paste0("^@", polHandle)))
  newDFL10 <- bind_dfs(newDFL10, addDFL10) %>%
    mutate(replyLevel = 10L, origPost = id, Politician = polName)
  
  newDFL20 <- filter(newDFL20, !(id %in% newDFL10$id))
  
  # We are only interested in tweets replying to a politician comment, so get rid of
  # tweets that have NA in the replyToID field
  df <- allTweets %>%
    filter(!is.na(replyToID))
  
  
  dfAllLevels1 <- add_Levels(df = df, 
                             TmlDF = TmlDF, 
                             polName = polName, 
                             oldDFLevel1 = oldDFL1,
                             oldDFLevel2 = oldDFL2,
                             oldDFLevel3 = oldDFL3,
                             oldDFLevel4 = oldDFL4)
  
  
  
  restDF <- filter(df, !(id %in% dfAllLevels1$id))
  
  addDFL10 <- filter(restDF, str_detect(str_to_lower(restDF$text), paste0("^@", polHandle)))
  newDFL10 <- bind_dfs(newDFL10, addDFL10) %>%
    mutate(replyLevel = 10L, origPost = id, Politician = polName) %>%
    bind_dfs(dfOld = oldDFL10)
  
  newDFL20 <- filter(newDFL20, !(id %in% newDFL10$id)) %>%
    mutate(replyLevel = 20L, Politician = polName) %>%
    bind_dfs(dfOld = oldDFL20)
  
  
  
  restDF <- filter(restDF, !(id %in% newDFL10$id))
  
  
  dfAllLevels2 <- add_Levels(df = restDF, 
                             TmlDF = newDFL10, 
                             polName = polName, 
                             baseLevel = 10L,
                             oldDFLevel1 = oldDFL11,
                             oldDFLevel2 = oldDFL12,
                             oldDFLevel3 = oldDFL13,
                             oldDFLevel4 = oldDFL14)
  
  
  
  newDFL20 <- filter(restDF, !(id %in% dfAllLevels2$id)) %>%
    mutate(replyLevel = 20L, Politician = polName) %>%
    bind_dfs(dfOld = newDFL20)
  
  dfAllLevels3 <- bind_rows(newDFL10, newDFL20)
  
  dfAllLevels <- bind_rows(dfAllLevels1, dfAllLevels2) %>%
    bind_rows(dfAllLevels3)
  
  
  
  ### Checks before exiting ###-------------------------------------------
  for (i in c(1:4, 11:14)) {
    tdf1 <- filter(dfAllLevels, replyLevel == (i-1))
    if (i == 1) {
      tdf1 <- TmlDF
    }
    tdf2 <- filter(dfAllLevels, replyLevel == i)
    
    if (!(all(tdf2$replyToID %in% tdf1$id))) {
      msg <- paste0("Politician: ", polName, ".  Missing replyTo at level ", i)
      warning(msg)
    }
  }
  rm(tdf1, tdf2)
  
  return(dfAllLevels)
}



add_Levels <- function(df, TmlDF, baseLevel = 0L, polName, oldDFLevel1, oldDFLevel2, oldDFLevel3, oldDFLevel4) {
  
  if (is_empty(TmlDF) || nrow(TmlDF) == 0L) return(NULL) 
  
  if (is_empty(df) || nrow(df) == 0L) {
    dfAllLevels <- bind_rows(oldDFLevel1, oldDFLevel2) %>%
      bind_rows(oldDFLevel3) %>%
      bind_rows(oldDFLevel4)
    
    return(dfAllLevels)
  }
  
  polPosts <- TmlDF$id
  
  dfAllLevels <- NULL
  # add new comments to the level1 comments
  dfL1 <- filter(df, replyToID %in% polPosts) %>% { 
    if (!(is.null(.) || nrow(.) == 0)) {
      mutate(., replyLevel = 1L + baseLevel, origPost = replyToID, Politician = polName)
    }
  }  %>%
    bind_dfs(dfOld = oldDFLevel1, checkSampled = TRUE) %>%
    unique()
  
  
  dfAllLevels <- dfL1 
  
  replCommentsL1 <- setdiff(dfL1$id, polPosts)
  
  # add new comments to the level2 comments
  if (!is_empty(replCommentsL1)) {
    dfL2 <- filter(df, replyToID %in% replCommentsL1) %>% { 
      if (!(is.null(.) || nrow(.) == 0)) {
        mutate(., replyLevel = 2L + baseLevel, origPost = get_origPost(replyToID, df = dfL1), Politician = polName)
      }
    } %>%
      bind_dfs(dfOld = oldDFLevel2, checkSampled = TRUE) %>%
      unique()
    
    dfAllLevels <- bind_rows(dfAllLevels, dfL2)
    
    replCommentsL2 <- setdiff(dfL2$id, c(replCommentsL1, polPosts))
    
    # add new comments to the level3 comments
    if (!is_empty(replCommentsL2)) {
      dfL3 <- filter(df, replyToID %in% replCommentsL2) %>% { 
        if (!(is.null(.) || nrow(.) == 0)) {
          mutate(., replyLevel = 3L + baseLevel, origPost = get_origPost(replyToID, df = dfL2), Politician = polName)
        }
      } %>%
        bind_dfs(dfOld = oldDFLevel3, checkSampled = TRUE) %>%
        unique()
      
      dfAllLevels <- bind_rows(dfAllLevels, dfL3)
      
      replCommentsL3 <- setdiff(dfL3$id, c(replCommentsL2, replCommentsL1, polPosts))
      
      # add new comments to the level4 comments
      if (!is_empty(replCommentsL3)) {
        dfL4 <- filter(df, replyToID %in% replCommentsL3) %>% { 
          if (!(is.null(.) || nrow(.) == 0)) {
            mutate(., replyLevel = 4L + baseLevel, origPost = get_origPost(replyToID, df = dfL3), Politician = polName)
          }
        } %>%
          bind_dfs(dfOld = oldDFLevel4, checkSampled = TRUE) %>%
          unique()
        
        dfAllLevels <- bind_rows(dfAllLevels, dfL4)
      }
    }
  }
  
  
  return(dfAllLevels)
}


#########################################################################################
# # In case we already have the downloaded to Comments, and just want to update the 4 levels:
# update_pol4Levels <- function(polDF = NULL, polTml = NULL, newToCom = "CleanedtoComments.RData") {
#   
#   if (is.null(polDF)) {
#     polDF <- get_AllPolDF()
#   } 
#   
#   # Get the politician Timelines (file with politician posts)
#   if (is.null(polTml)) {
#     polTml <- get_PolTimeline(dfOnly = TRUE)
#   } 
#   
#   # newToCom can be character (the filename to open)
#   if (is.character(newToCom)) {
#     newToCom <- get_atComments(newToCom) %>%
#       removeNull(isDF = TRUE)
#   } 
#   
#   # or a list, or NULL.  When Null, stop and send a warning
#   if (is_empty(newToCom)) {
#     simpleWarning("No comments list supplied")
#     return(NULL)
#   }
#   
#   listNames <- names(newToCom)
#   
#   # get the file with the comments by level made up to date
#   newPolLevels <- get_PolLevels(numPols = nrow(polDF), listN = polDF$listName)
#   
#   l_ply(listNames, function(i) {
#     print(paste0("Now working on list name: ", i))
#     df2 <- newToCom[[i]]
#     
#     pHandle <- get_PolHandle(pName = i, polDF = polDF)
#     isNullDF2 <- is_empty(df2) || nrow(df2) == 0
#     
#     if (!isNullDF2) {
#       
#       newPolLevels[[i]] <<- make_4Levels(allTweets = df2, 
#                                          polHandle = pHandle, 
#                                          polTml = polTml,
#                                          oldPolLvl = newPolLevels[[i]], 
#                                          polDF = polDF)
#     }
#   })
#   
#   
#   # Using lubridate to create dates of the form "ddmmyyyy"  and save backup
#   f <- stamp_date("31092018")
#   backupPath <- paste0("Backup/", f(Sys.Date()), "_pol4Levels.RData")
#   save(newPolLevels, file = backupPath)
#   
#   # get rid of NULLS in newPolLevels
#   nullDF <- ldply(newPolLevels, function(df) {
#     if (is_empty(df) || nrow(df) == 0L) return(TRUE) else return(FALSE)}, .id = NULL)
#   
#   newPolLevels <- newPolLevels[!nullDF]
#   
#   ### save polLevels and backup
#   fPath <- file.path("Data", "TwData", "pol4Levels1.RData")
#   save(newPolLevels, file = fPath)
#   save(newPolLevels, file = backupPath)
#   
#   result <- newPolLevels
#   return(result)
# }

