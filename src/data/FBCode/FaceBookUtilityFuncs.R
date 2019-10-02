
# DATE CREATED: 30.11.2018
# This file contains various utility functions for the collection and storing of FB posts/comments


#########################################################################################
######################      LOAD Tokens       ###########################################
#########################################################################################

# Which of the stored FB Tokens should be used? 

load_Token <- function(n = 1) {
  useToken <- paste0("FBToken_", n)
  FBPath <- file.path("Data", "FBData", "Tokens", useToken)
  if (!file.exists(FBPath)) FBPath <- file.path("Data", "FBData", "Tokens", "FBToken_1")
  
  FBToken <- readRDS(FBPath)
  return(FBToken)
}





#########################################################################################
####################  Getting all Politician Names ######################################
#########################################################################################


# Returns a data frame with politician names and their twitter handles
get_FBPolDF <- function(fName = NULL) {
  if (is_empty(fName)) fName <- "List_of_Politicians.csv"
  
  polFile <- file.path("PoliticianList", fName)
  if (!file.exists(polFile)) polFile <- file.path("..", "Collect Comments", polFile)
  
  polDF <- read_csv(file = polFile)
  
  polFBHandles <- polDF$FB_Confirmed %>%
    str_replace("https://www.facebook.com/", "") %>%
    str_replace_all("/", "") %>%
    str_replace(".+-", "") %>%
    str_replace("\\?ref=br_rs", "")
  
  lName <- str_replace_all(polDF$Name, " ", "_") %>%
    str_replace_all("Ä", "AE") %>%
    str_replace_all("Ö", "OE") %>%
    str_replace_all("Ü", "UE") %>%
    str_replace_all("[^A-z0-9_]", "")
  
  returnDF <- mutate(polDF, polHandles = polFBHandles, listName = lName) %>%
    filter(!(polHandles  %in% c("no", "nicht vorhanden"))) %>%
    select(Name, polHandles, listName) %>%
    drop_na(polHandles)
  
  return(returnDF)
}


# Given the FB handle, find the politician name
get_FBPolName <- function(FBHandle, polDF = NULL) {
  if (is.null(polDF)) polDF <- get_AllPolDF("FB")
  pName <- polDF$Name[str_to_lower(polDF$polHandles) == str_to_lower(FBHandle)]
  return(pName)
}


# Given the FB handle, find the list name (pol name without special characters)
get_FBlistName <- function(FBHandle, polDF = NULL) {
  if (is.null(polDF)) polDF <- get_AllPolDF("FB")
  lName <- polDF$listName[str_to_lower(polDF$polHandles) == str_to_lower(FBHandle)]
  return(lName)
}


# Given the politician name, or the list name, find the corresponding twitter handle
get_FBPolHandle <- function(pName, polDF = NULL) {
  
  if (is.null(polDF)) polDF <- get_AllPolDF("FB")
  FBHandle <- NULL
  
  if (is_empty(pName)) return(NULL)
  
  if (pName %in% polDF$Name) {
    FBHandle <- polDF$polHandles[str_to_lower(polDF$Name) == str_to_lower(pName)]
  } else if (pName %in% polDF$listName) {
    FBHandle <- polDF$polHandles[str_to_lower(polDF$listName) == str_to_lower(pName)]
  }
  
  return(FBHandle)
}



#########################################################################################
#########  Get the most recent politician feeds, and join to existing file ##########
#########################################################################################

# This function returns the Politician Feeds in FBFeedFile
# returnRes can be:
##  "All"  --> returns everything in the FBFeedFile
##  "Feed" --> returns only the DF with the Politician posts collected so far
##  "Log"  --> returns only the error Log with info on Politicians where errors occurred in downloading data so far

get_PolFeed <- function(FBFeedFile = "CleanedPolFeeds.RData", startDate = NULL, returnRes = "All") {
  
  if (is.null(startDate)) startDate <- firstDay
  
  FBFeedPath <- file.path("Data", "FBData", FBFeedFile)
  if (!file.exists(FBFeedPath)) FBFeedPath <- file.path("..", "Collect Comments", FBFeedPath)
  
  
  if (!is_empty(FBFeedPath)) {   
    attach(FBFeedPath)
    cleanedFBPosts <- cleanedFBPosts
    firstNum <- firstNum
    sinceDate <- sinceDate
    untilDate <- untilDate
    errorLog <- errorLog
    detach(paste0("file:", FBFeedPath), character.only = TRUE)
  } else {
    cleanedFBPosts <- NULL
    firstNum <- 1
    sinceDate <- startDate
    untilDate <- Sys.Date() - 2
    errorLog <- list()
  }
  
  switch(returnRes,
         "Feed" = return(cleanedFBPosts),
         "Log" = return(errorLog),
         "Status" = {
           results <- list(firstNum = firstNum,
                           sinceDate = sinceDate,
                           untilDate = untilDate,
                           errorLog = errorLog)
           return(results)
         },
         {results <- list(cleanedFBPosts = cleanedFBPosts,
                          firstNum = firstNum,
                          sinceDate = sinceDate,
                          untilDate = untilDate,
                          errorLog = errorLog)
         return(results)
         })
  
}


save_PolFeed <- function(cleanedFBPosts, fName = "CleanedPolFeeds.RData", ...) {
  # save the combined FBPosts (old + new)
  timeStamp <- as.integer(Sys.time())
  
  FBFeedPath <- file.path("Data", "FBData", fName)
  BackupPath <- paste0("Backup/", timeStamp, "_", fName)
  
  save(cleanedFBPosts, ..., file = BackupPath)
  save(cleanedFBPosts, ..., file = FBFeedPath)
  return(invisible())
}


# This function takes the list to transform into a DF, and other information to save in the
# CleanedPolFeeds file
# minFileNum ... in case we have to add politicians later, when 
# lower File Numbers have already had all their comments collected
addTo_PolFeed <- function(PostList, FBFeedFile = "CleanedPolFeeds.RData", minFileNum = 1L, ...) {
  
  startDate <- firstDay
  limitDate <- lastPostDay
  
  PostList <- removeNull(PostList)
  FBPostsNew <- bind_rows(PostList, .id = "Politician")
  
  if (is_empty(FBPostsNew) || nrow(FBPostsNew) == 0L) return(NULL)
 
  #filter(FBPostsNew, !is.na(user))
  cleanedFBPostsNew <- FBPostsNew %>%
#    filter(!is.na(text), !(str_squish(text) %in% c("", " "))) %>%  # don't remove empty text - could be deleted comments
    clean_FBCommentsDF(op = "select") %>%
    mutate(CommentsAvail = as.Date(dateCreated),   # we need to set some markers to determine if 
           RepAvail = as.Date(dateCreated),        # all the comments and replies have been downloaded
           FileNum = (as.integer(as.Date(dateCreated) - startDate) %/% 3) + 1) %>%
    mutate(FileNum = ifelse(FileNum < minFileNum, minFileNum, FileNum),
           Level = ifelse(is.na(user), 10L, Level))    # where are the comments and replies saved? FBComs<FileNum>.RData & FBReps<FileNum>.RData
  
  ## get the old FBPosts
  
  cleanedFBPosts <- get_PolFeed(FBFeedFile, returnRes = "Feed")
  
  ## add the new FBPosts to the old
  cleanedFBPosts <- bind_FBdfs(cleanedFBPosts, cleanedFBPostsNew) %>%
    filter(as.Date(dateCreated) < limitDate)   # we keep only posts from before Oct. 4, 2019
  
  save_PolFeed(cleanedFBPosts, fName = FBFeedFile, ...)
  
  return(cleanedFBPosts)
}




#########################################################################################
#############  Get Posts from Politicians' Public Pages #################################
#########################################################################################

get_FBPosts <- function(name, num = 1000, sinceDate = NULL, untilDate = NULL, FBToken) {
  
  if (is.na(name) | name == "private") return(NULL)
  
  if (is.null(sinceDate)) {
    sinceDate <- Sys.Date() - 6
  }
  
  if (is.null(untilDate)) {
    untilDate <- Sys.Date() - 2
  }
  
  print(paste0("get_FBPosts ", name, ": sinceDate - ", sinceDate, "; untilDate - ", untilDate))
  
  searchTerm <- name 
  
  currentPage <- tryCatch(myGetPage(page = searchTerm,
                                    token = FBToken,
                                    n = num,
                                    since = sinceDate,
                                    until = untilDate,
                                    reactions = TRUE,
                                    feed = TRUE),
                          error = function(e) {
                            print(e)
                            msg <- as.character(simpleError(e))
                            errorInfo <- list(Pol = name, Msg = msg, Time = Sys.time())
                            errorLog[[name]] <<- errorInfo
                            if (str_detect(msg, "request limit reached") == TRUE) {
                              Sys.sleep(1800) # stop for 30 minutes
                            } else {
                              Sys.sleep(90)
                            }
                            return(NULL)
                          })
  
  if (is_empty(currentPage) || nrow(currentPage) == 0) return(NULL)
  
  currentPage <- currentPage %>%
    select(user = from_name, text = message, created = created_time, everything(), -type, -link, -story) %>%
    mutate(Deleted = FALSE)

  # Let the system sleep depending on length of current page:
  # In fact, getPage has to call the API every 25 posts, so let it rest 3 secs for every API call:
  numRounds <- ceiling(nrow(currentPage)/25)
  Sys.sleep(3*numRounds)
  
  return(currentPage)
}


#########################################################################################
###########  Get Comments to Posts on Politicians' Public Pages #########################
#########################################################################################

# attach the latest downloaded comments:

get_DownloadedComments <- function(comFile = "FBComs", FN = "", comOnly = FALSE) {
  
  fName <- paste0(comFile, FN, ".RData")
  fPath <- file.path("Data", "FBData", fName)
  if (file.exists(fPath)) {
    attach(fPath)
    comDF <- comDF
    errorLogC <- get0("errorLogC", envir = .GlobalEnv, ifnotfound = list())
    detach(paste0("file:", fPath), character.only = TRUE)
  } else {
    comDF <- NULL
    errorLogC <- list()
  }
  
  if (comOnly == TRUE) return(comDF)
  
  return(list(comDF = comDF, 
              errorLogC = errorLogC))
}



get_DownloadedReplies <- function(repFile = "FBReps", FN = "", repOnly = FALSE) {
  
  fName <- paste0(repFile, FN, ".RData")
  fPath <- file.path("Data", "FBData", fName)
  if (file.exists(fPath)) {
    attach(fPath)
    comDFavail <- comDFavail
    repDF <- repDF
    errorLogR <- errorLogR
    detach(paste0("file:", fPath), character.only = TRUE)
  } else {
    comDFavail <- NULL
    repDF <- NULL
    errorLogR <- list()
  }
  
  if (repOnly == TRUE) return(repDF)
  
  return(list(comDFavail = comDFavail, repDF = repDF, 
              errorLogR = errorLogR))
}


save_Com <- function(comDF, errorLogC, FN = "", fName = "FBComs", allDone = FALSE) {
  timeStamp <- as.integer(Sys.time())
  fName <- paste0(fName, FN, ".RData")
  oldPath <- file.path("Data", "FBData", fName)
  if (allDone == TRUE) {
    fPath <- file.path("Data", "FBData", "ready4Sampling", fName)
    if (file.exists(oldPath)) file.remove(oldPath)
  } else {
  fPath <- oldPath
  }
  save(comDF, errorLogC, 
       file = fPath)
  save(comDF, errorLogC, 
       file = paste0("Backup/", timeStamp, "_", fName))
  return(invisible())
}

save_Rep <- function(comDFavail, repDF, errorLogR, FN = "", fName = "FBReps", allDone = FALSE) {
  timeStamp <- as.integer(Sys.time())
  print(fName)
  fName <- paste0(fName, FN, ".RData")
  print(fName)
  if (allDone == TRUE) {
    fPath <- file.path("Data", "FBData", "ready4Sampling", fName)
    file.remove(file.path("Data", "FBData", fName))
  } else {
  fPath <- file.path("Data", "FBData", fName)
  }
  save(comDFavail, repDF, errorLogR, 
       file = fPath)
  save(comDFavail, repDF, errorLogR,  
       file = paste0("Backup/", timeStamp, "_", fName))
  return(invisible())
}



save_sampledComReps <- function(sampledDF, comDF, repDF,
                               FN = "", fName = "FBComReps", fNameCom = "FBComs", fNameRep = "FBReps") {
  timeStamp <- as.integer(Sys.time())
  fName <- paste0(fName, FN, ".RData")
  fNameCom <- paste0(fNameCom, FN, ".RData")
  fNameRep <- paste0(fNameRep, FN, ".RData")
  
  fPath <- file.path("Data", "FBData", "ready4Sampling", fName)
  fPathCom <- file.path("Data", "FBData", "Sampled", fNameCom)
  fPathRep <- file.path("Data", "FBData", "Sampled", fNameRep)
  
  save(sampledDF, file = fPath)
  save(sampledDF, file = paste0("Backup/", timeStamp, "_", fName))
  save(comDF, file = fPathCom)
  save(repDF, file = fPathRep)
  
  file.remove(file.path("Data", "FBData", "ready4Sampling", fNameCom))
  file.remove(file.path("Data", "FBData", "ready4Sampling", fNameRep))
  
  
  return(invisible())
}

update_ComAvail <- function(newComDF, FN = "") {
  if (is_empty(newComDF) || nrow(newComDF) == 0L) return(NULL)
  
  list2env(get_DownloadedReplies(FN), envir = environment())
  
  newDF <- bind_FBdfs(dfOld = comDFavail, dfNew = newComDF, checkAvail = TRUE, checkSampled = FALSE)
  save_Rep(comDFavail = newDF, repDF, errorLogR, fName = "FBReps", FN = FN)
}




get_FBComments <- function(postID, sinceDate = NULL, untilDate = NULL, number = 25000, politician, FBToken) {
  
  if (is.null(sinceDate)) sinceDate <- as.Date("2019-01-01")
  if (is.null(untilDate)) untilDate <- Sys.Date() - 2
  
  if (is_empty(postID) || is.na(postID)) return(NULL)
  
  print(paste0("get_FBComments, working on post number: ", postID))
  
  thisPost <- tryCatch(myGetPost(post = postID, minDate = sinceDate,
                                 n = number, n.comments = number, 
                                 likes = FALSE, reactions = TRUE, token = FBToken),
                       error = function(e) {
                         print(e)
                         msg <- as.character(simpleError(e))
                         errorInfo <- list(Pol = politician, Msg = msg, Post = postID, Time = Sys.time())
                         errorLogC[[postID]] <<- errorInfo
                         if (str_detect(msg, "request limit reached") == TRUE) {
                           Sys.sleep(1200) # stop for 20 minutes
                         } else {
                           Sys.sleep(90)
                         }
                         return(NULL)
                       })
  
  if (is.null(thisPost) || is.null(thisPost$comments) || nrow(thisPost$comments) == 0) return(NULL)
  
  postComments <- thisPost$comments %>%
    mutate(replyToID = postID, origPost = postID) %>%
    filter(as.Date(created_time) < untilDate)
  
  l <- nrow(postComments) %/% 100
  Sys.sleep(l)
  
  return(postComments)
}


#########################################################################################
##################  Get Replies to User Comments ########################################
#########################################################################################



get_FBReplies <- function(comment, numRep = 10000, untilDate = NULL) {
  
  print(paste0("get_FBReplies, number of replies to get is: ", numRep))
  if (is.null(untilDate)) untilDate <- Sys.Date() - 2
  
  numberR <- comment$comments_count
  replyPost <- comment$origPost
  Lvl <- comment$Level
  
  # If there are too many replies, cap with numRep
  
  if (numberR > numRep) numberR <- numRep
   
  
  # each element of getCommentReplies is itself a list of 2 items.
  # the first item is the comment, and info about it
  # the second item is a df of replies to that comment, and info about them
  # So we will use bind_rows to combine them into one dataframe
  
  commentID <- comment$id
  
  comRep <- tryCatch(myGetCommentReplies(commentID, token = FBToken, 
                                         replies = TRUE, 
                                         likes = FALSE, 
                                         n = numberR, 
                                         n.replies = numberR),
                     error = function(e) {
                       print(simpleError(e))
                       msg <- as.character(simpleError(e))
                       errorInfo <- list(Pol = comment$Politician, Msg = msg, Comment = commentID, Time = Sys.time())
                       errorLogR[[commentID]] <<- errorInfo
                       if (str_detect(msg, "request limit reached") == TRUE) {
                         Sys.sleep(1200) # stop for 20 minutes
                       } else if (str_detect(msg, "Unsupported get request")) {
                         replyPost <<- commentID
                         Sys.sleep(5)
                         return("REMOVED")
                       } else Sys.sleep(90)
                        return(NULL)
                     })
  
  if (is.null(comRep)) return(NULL)
  if (any(comRep == "REMOVED", na.rm = TRUE)) {
    commentReplies <- tibble(text = "[LEVEL 1 COMMENT AND REPLIES REMOVED]", 
                                 Level = 1 + Lvl,
                                 replyToID = commentID,
                             origPost = replyPost,
                             Politician = comment$Politician,
                             replyToUser = "Anonymous")
    return(commentReplies)
  }
  
  commentReplies <- bind_rows(comRep)
  
  commentReplies$replyToID <- commentID
  commentReplies$Level <- Lvl + 1
  commentReplies$origPost <- replyPost
  commentReplies$replyToID[1] <- replyPost
  commentReplies$Level[1] <- Lvl
  commentReplies$Politician <- comment$Politician
  commentReplies$replyToUser <- comment$user

  commentReplies <- rename(commentReplies, user = from_name, text = message, created = created_time) %>%
    filter(as.Date(created) < untilDate)
  
  print("end of Fb replies")
  return(commentReplies)
}




#########################################################################################
####################  Anonymize User Comments ###########################################
#########################################################################################

# Hash the id in v if name is not one of the politician Twitter Handles
FBhash_id <- function(v, name, polDF = NULL, otherPols = NULL) {
  if (is_empty(v) || all(is.na(v)) || all(v == "")) return(NA)
  if (is.null(polDF)) polDF <- get_AllPolDF("FB")
  foundPols <- c(polDF$Name, polDF$polHandles, polDF$listName, otherPols)
  searchPols <- str_to_lower(foundPols)
  
  set.seed(secretSeed)
  rnum <- as.raw(sample(100, 1))
  newV <- ifelse(str_to_lower(name) %in% searchPols, v, sha256(x = v, key = rnum))
  
  return(newV)
}


# Hash the id in v if name is not one of the politician Twitter Handles
FBhash_origPost <- function(v, Level) {
  if (is_empty(v) || all(is.na(v)) || all(v == "")) return(NA)

  set.seed(secretSeed)
  rnum <- as.raw(sample(100, 1))
  newV <- ifelse(Level < 10L, v, sha256(x = v, key = rnum))
  
  return(newV)
}

#########################################################################################
####################  Clean FaceBook Data Frame #########################################
#########################################################################################

# This function will return a dataframe with cleaned up data retrieved from Twitter.
# Sensitive user names and ids will be removed - only politicians from the list will remain (this is what the term anonymized means)
# sensitive status IDs (the id of the tweet itself) will be hashed (only the politicians' from the list will remain unchanged)

# What to keep:
# user --> anonymized
# userID = from_id  --> hashed
# text = text  --> anonymized
# id  --> hashed
# replyToUser = reply_to_screen_name --> anonymized
# replyToID = reply_to_status_id --> hashed
# dateCreated = created
# favoriteCount = likes_count
# shareCount = shares_count
# comments_count
# love_count
# haha_count
# wow_count
# sad_count
# angry_count

# op can be "all", "select", "hash", and/or "anon"

clean_FBCommentsDF <- function(df, polDF = NULL, morePols = NULL, Level = 0L, op = c("anon", "select")){
  
  if (is.null(Level)) Level <- df$Level
  if (is.null(polDF)) polDF <- get_AllPolDF("FB")
  searchPols <- c(polDF$polHandles, morePols) %>%
    str_to_lower() 
  
  newDF <- df

  # select the variables we want to keep
  if (any(op %in% c("all", "select"))) {
    if (all(Level == 0L)) {
    newDF <- newDF %>%
      select(
        Politician,
        user,
        userID = from_id,
        text,
        id,
        dateCreated = created,
        favoriteCount = likes_count,
        shareCount = shares_count,
        comments_count,
        love_count,
        haha_count,
        wow_count,
        sad_count,
        angry_count
      ) %>%
      mutate(replyToID = NA, replyToUser = NA, origPost = id, Level = Level, Sampled = "N")  # Add this, so we know whether a comment has already been sampled or not
    } else {
      newDF <- newDF %>%
        select(
          Politician,
          user,
          userID = from_id,
          text,
          id,
          replyToID,
          replyToUser,
          dateCreated = created,
          favoriteCount = likes_count,
          comments_count,
          origPost
        ) %>%
        mutate(Level = Level, Sampled = "N")  # Add this, so we know whether a comment has already been sampled or not
    }
  }
  
  # hash IDs
  if (any(op %in% c("all", "hash")) && any(Level > 0L)) {
    print("hashing")
    newDF <- newDF %>%
      mutate(
        id = FBhash_id(id, user, polDF = polDF, otherPols = morePols),
        replyToID = FBhash_id(replyToID, replyToUser, polDF = polDF, otherPols = morePols)
      ) 
  }
  
  # Anonymize generic user names
  if (any(op %in% c("all", "anon"))) {
    newDF <- newDF %>%
      mutate(
        user = ifelse(Level == 0L, user, 
                      ifelse(str_to_lower(user) %in% searchPols, user, "Anonymous")),
        userID = ifelse(Level == 0L, userID, 
                      ifelse(str_to_lower(user) %in% searchPols, userID, NA)),
        replyToUser = ifelse(Level == 1L, replyToUser, 
                             ifelse(str_to_lower(replyToUser) %in% searchPols, replyToUser, "Anonymous"))
      ) 
  }
  
  return(unique(newDF))
}


#########################################################################################
####################  JOINING Files #####################################################
#########################################################################################

# Given two FBPost/Comment df's, dfOld and dfNew, determine which elements of dfOld are duplicated in dfNew
# For this function, duplicated means the comment ID is the same
# Return the df consisting of the binding by row of dfOld and dfNew, removing first the rows
# in dfOld that are duplicated in dfNew
# if checkAvail is TRUE, check which elements in newDF were already in the oldDF, and had CommentsAvail == TRUE

# Helper function 
choose_Date <- function(xDate, yDate) {
  l <- length(xDate)
  if (l != length(yDate)) return(NULL)
  zDate <- ldply(1:length(xDate), function(i) {
    if (is.na(yDate[i]) || yDate[i] < xDate[i]) return(xDate[i])
    return(yDate[i])
  })
  zDate <- zDate$V1
}

bind_FBdfs <- function(dfOld, dfNew, checkAvail = FALSE, checkSampled = FALSE) {
  
  if (is_empty(dfOld) || nrow(dfOld) == 0) return(dfNew)
  
  
  isDupID <- !is.na(dfOld$id) & dfOld$id %in% dfNew$id
  
  dfOld2 <- filter(dfOld, !isDupID)
  
  newDF <- bind_rows(dfOld2, dfNew) %>%
    unique()
  
  # check here which elements in newDF were already in the oldDF, and had CommentsAvail = TRUE
  if (checkAvail == TRUE) {  
    if (is_empty(newDF) || (nrow(newDF) == 0)) return(NULL)

    oldDF <- filter(dfOld, isDupID)
    if (is_empty(oldDF) || (nrow(oldDF) == 0)) return(newDF)
    

    if ("CommentsAvail" %in% names(newDF)) {
      oldDF <- select(oldDF, id, CommentsAvail)
      newDF <- left_join(newDF, oldDF, by = "id") %>%
        mutate(CommentsAvail = ifelse(is.na(CommentsAvail.y), CommentsAvail.x,
                                      ifelse(as.Date(CommentsAvail.x) > as.Date(CommentsAvail.y), CommentsAvail.x, CommentsAvail.y))) %>%
        select(-CommentsAvail.x, -CommentsAvail.y)
    }
    
    if ("RepAvail" %in% names(newDF)) {
      oldDF <- select(oldDF, id, RepAvail)
      newDF <- left_join(newDF, oldDF, by = "id") %>%
        mutate(RepAvail = ifelse(is.na(RepAvail.y), RepAvail.x, 
                                 ifelse(as.Date(RepAvail.x) > as.Date(RepAvail.y), RepAvail.x, RepAvail.y))) %>%
        select(-RepAvail.x, -RepAvail.y)
    }
    
    if ("repAvail" %in% names(newDF)) {
      oldDF <- select(oldDF, id, repAvail)
      newDF <- left_join(newDF, oldDF, by = "id") %>%
        mutate(repAvail = choose_Date(repAvail.x, repAvail.y)) %>%
        select(-repAvail.x, -repAvail.y)
    }
    
  }
  
  # check here which elements in newDF were already in the oldDF, and had Sampled = "Y"
  if (checkSampled == TRUE) {  
    if (is_empty(newDF) || (nrow(newDF) == 0)) return(NULL)
    oldDF <- dfOld 
    if (is_empty(oldDF) || (nrow(oldDF) == 0)) return(newDF)
    oldDF <- select(oldDF, id, Sampled)
    newDF <- left_join(newDF, oldDF, by = "id") %>%
      mutate(Sampled = ifelse(is.na(Sampled.y), Sampled.x, 
                              ifelse(Sampled.y > Sampled.x, Sampled.y, Sampled.x))) %>%  # Because Y > P > N
      select(-Sampled.x, -Sampled.y)
  }
  
  return(newDF)
}



