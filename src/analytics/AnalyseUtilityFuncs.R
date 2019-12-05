

# Given the Twitter Politician TimeLine, create a DF with minimum and maximum Twitter ID
# for each day in the Timeline.
# This can be used to determine the day a tweet was created based on twitter ID, even if dateCreated is NULL or NA

create_DateDictionary <- function(cleanPolTL) {
  minDate <- as.Date(min(cleanPolTL$dateCreated))
  maxDate <- as.Date(max(cleanPolTL$dateCreated))
  numDays <- as.integer(maxDate - minDate)
  
  dateLookUp <- ldply(0:numDays, function(d) {
    useDate <- minDate + d
    useID <- get_BoundaryID(useDate, polTL = cleanPolTL, includeDF = FALSE)
    useMinID <- useID$minID
    useMaxID <- useID$maxID
    
    if (is_empty(useMaxID)) {
      maxLen <- max(nchar(cleanPolTL$id), nchar(useMinID), 0, na.rm = TRUE)
      newDF <- mutate(cleanPolTL, padded_status_id = pad_id(id, maxLen)) 
      useMinID <- pad_id(useMinID, maxLen)
      
      useMaxID <- max(newDF$padded_status_id, useMinID, na.rm = TRUE)  # the max ID in the current data set
    }
    
    result <- tibble(Date = useDate, minID = useMinID, maxID = useMaxID)
    return(result)
  })
  
  return(dateLookUp)
}

# Given a string of Twitter IDs, and the Date Dictionary based on the Twitter Timeline,
# find the day a tweet with given ID was created

add_Date <- function(currID, lookUp) {
  maxLen <- max(nchar(lookUp$minID), nchar(lookUp$maxID), nchar(currID))
  lookUp <- mutate(lookUp, minID = pad_id(minID, maxLen), maxID = pad_id(maxID, maxLen))
  l <- nrow(lookUp)
  
  absMin <- lookUp$minID[1]
  absMax <- lookUp$maxID[l]
  
  gDate <- ldply(currID, function(i) {
    res <- lookUp %>%
      filter(minID < i, maxID > i) %>%
      pull(Date)
    
    if (is_empty(res)) {
      if (i < absMin) res <- NA
      if (i >= absMax) res <- lookUp$Date[l]
    }
    return(res)
  })
  return(gDate[[1]])
}



# Load the Facebook data, which is spread over several files
###### FB Stuff:

load_FBComments <- function(getRep = FALSE) {
  FBPath <- file.path("../Collect Comments/Data/FBData")
  polDF <- get_AllPolDF(Site = "FB")
  
  (fComs <- get_fileNames())
  # Download all the FBComments files - we'll count them file by file!
  FBComs <- ldply(fComs, bind_FBComments, polDF = polDF)
  
  if (getRep == TRUE) {
    (fReps <- get_fileNames(fPattern = "FBReps\\d{1,2}"))
    FBReps <- ldply(fReps, bind_FBComments, getComs = FALSE, polDF = polDF)  
    
    FBComments <- FBReps %>%
      filter(!(id %in% FBComs$id)) %>%
      bind_rows(FBComs) %>%
      unique()
  } else {
    FBComments <- unique(FBComs)
  }
  
  return(FBComments)
}



get_fileNames <- function(fDir = NULL, fPattern = "FBComs\\d{1,2}") {
  
  if (is_empty(fDir)) {
    FBPath <- file.path("../Collect Comments/Data/FBData") 
    FBPath1 <- file.path(FBPath, "Sampled")
    FBPath2 <- file.path(FBPath, "ready4Sampling")
    fDir <- c(FBPath1, FBPath2)
  }
  fNames <- c()
  
  for (sDir in fDir) {
    fileName <- dir(sDir) %>%
      str_subset(fPattern) 
    
    fileName <- file.path(sDir, fileName)
    fNames <- c(fNames, fileName)
  }
  
  return(fNames)
}


#### open the available FBComs files and get the comments count info
bind_FBComments <- function(f, getComs = TRUE, polDF = NULL) {
  if (!file.exists(f)) return(NULL)
  if (is_empty(polDF)) polDF <- get_AllPolDF(Site = "FB")
  
  usePols <- str_to_lower(polDF$polHandles)
  
  print(f)
  attach(f)
  
  if (getComs == TRUE) fbDF <- comDF else {
    fbDF <- repDF
    if (!(is_empty(fbDF) || nrow(fbDF) == 0)) {
      fbDF <- filter(repDF, !(str_detect(text, "LEVEL 1 COMMENT AND REPLIES REMOVED")))
    }
  }
  
  detach(paste0("file:", f), character.only = TRUE)
  
  if (!(is_empty(fbDF) || nrow(fbDF) == 0)) {
    fbDF <- mutate(fbDF, 
                   id = FBhash_id(id, user, polDF = polDF),
                   replyToID = FBhash_id(replyToID, replyToUser, polDF = polDF),
                   origPost = FBhash_origPost(origPost, Level),
                   dateCreated = ymd_hms(dateCreated)) %>%
      select(Politician, user, id, replyToUser, replyToID, origPost, replyCount = comments_count, favoriteCount, Level, text, dateCreated) 
  }
  
  return(fbDF)
}



count_Comments <- function(df) {
  if (is_empty(df) || nrow(df) == 0L) return(NULL)
  
  # don't double count messages that are contained in more than one thread
  df1 <- select(df, textID, replyToID) %>%
    unique()
  
  # don't double count messages that are sent to more than one recipient in the same thread
  df2 <- select(df, textID, origPost) %>%
    unique()
  
  res1 <- count(df1, replyToID, name = "replyCount") 
  res2 <- count(df2, origPost, name = "commentCount")
  newDF <- left_join(df, res1, by = c("textID" = "replyToID")) %>%
    replace_na(list("replyCount" = 0L))
  newDF <- left_join(newDF, res2, by = c("textID" = "origPost")) %>%
    replace_na(list("commentCount" = 0L)) %>%
    mutate(commentCount = ifelse(commentCount > 0L, commentCount - 1, 0L))
  
  return(newDF)
}

