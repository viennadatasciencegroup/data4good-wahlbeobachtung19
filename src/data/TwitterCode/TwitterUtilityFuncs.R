
# DATE CREATED: 12.11.2018
# This file contains various utility functions for the collection and storing of Tweets



#########################################################################################
####################  Getting all Politician Names ######################################
#########################################################################################



# Returns a data frame with politician names and their twitter handles
get_TwPolDF <- function(fName = NULL) {
  if (is_empty(fName)) fName <- "List_of_Politicians.csv"
  
  polFile <- file.path("PoliticianList", fName)
  if (!file.exists(polFile)) polFile <- file.path("..", "Collect Comments", polFile)
  
  polDF <- read_csv(file = polFile)

  polTwHandles <- polDF$Twitter_Confirmed %>%
    str_replace("https://twitter.com/", "") %>%
    str_replace_all("/", "") %>%
    str_to_lower()
  
  lName <- str_replace_all(polDF$Name, " ", "_") %>%
    str_replace_all("Ä", "AE") %>%
    str_replace_all("Ö", "OE") %>%
    str_replace_all("Ü", "UE") %>%
    str_replace_all("[^A-z0-9_]", "")
  
  returnDF <- mutate(polDF, polHandles = polTwHandles, listName = lName) %>%
    drop_na(polHandles) %>%
    filter(!(polHandles  %in% c("no", "nicht vorhanden"))) %>%
    select(Name, polHandles, listName)
  
  return(returnDF)
}


# Given the twitter handle or list name, find the politician name
get_TwPolName <- function(TwHandle, polDF = NULL) {
  TwHandle <- str_replace_all(TwHandle, "@", "") %>%
    str_to_lower()
  if (is.null(polDF)) polDF <- get_AllPolDF()
  if (TwHandle %in% polDF$polHandles) {
  pName <- polDF$Name[str_to_lower(polDF$polHandles) == str_to_lower(TwHandle)]
  } else {
    pName <- polDF$Name[str_to_lower(polDF$listName) == str_to_lower(TwHandle)]
  }
  
  return(pName)
}


# Given the twitter handle or politician name, find the list name (pol name without special characters)
get_TwlistName <- function(TwHandle, polDF = NULL) {
  TwHandle <- str_replace_all(TwHandle, "@", "") %>%
    str_to_lower()
  if (is.null(polDF)) polDF <- get_AllPolDF()
  if (TwHandle %in% str_to_lower(polDF$polHandles)) {
  lName <- polDF$listName[str_to_lower(polDF$polHandles) == TwHandle]
  } else {
    lName <- polDF$listName[str_to_lower(polDF$Name) == TwHandle]
  }
  return(lName)
}


# Given the politician name, or the list name, find the corresponding twitter handle
get_TwPolHandle <- function(pName, polDF = NULL) {
  
  if (is.null(polDF)) polDF <- get_AllPolDF()
  TwHandle <- NULL
  
  if (is_empty(pName)) return(NULL)
  
  if (pName %in% polDF$Name) {
    TwHandle <- polDF$polHandles[str_to_lower(polDF$Name) == str_to_lower(pName)]
  }else if (pName %in% polDF$listName) {
    TwHandle <- polDF$polHandles[str_to_lower(polDF$listName) == str_to_lower(pName)]
  }
  
  return(TwHandle)
}


#########################################################################################
####################  WORKING with Tweet IDs ############################################
#########################################################################################

# pad the tweet id string with initial zeros so that all tweet id's have the same length
# this is necessary so that a max/min comparison will work
# otherwise: max("9", "11") gives "9", but max("09", "11") gives "11"

pad_id <- function(s, maxLen) {
  if (is_empty(s) || length(s) == 0L) return(NULL)
  newS <- vapply(s, function(x) {
    numChars <- if (is.na(x)) 0 else nchar(x)
    paste0(paste0(rep("0", maxLen - numChars), collapse = ""), x, collapse = "")
  }, FUN.VALUE = "1")
}


#########################################################################################
######################  Find Min/Max Dates in Comments Files ############################
#########################################################################################

find_minDates <- function(min = TRUE, fName = NULL, cList = NULL) {
  if (is.null(cList)) cList <- get_atComments(fName)
  
  if (min == TRUE) {
  minDates <- ldply(cList, function(df) {
    vDates <- if ("dateCreated" %in% names(df)) df$dateCreated else df$created_at
    minDate <- min(vDates, na.rm = TRUE) 
  }, .id = "listName") %>%
    rename(Min_Date = V1)
  } else {
    minDates <- ldply(cList, function(df) {
      vDates <- if ("dateCreated" %in% names(df)) df$dateCreated else df$created_at
      minDate <- max(vDates, na.rm = TRUE) 
    }, .id = "listName") %>%
      rename(Max_Date = V1)
  }
  
  return(minDates)
}


#########################################################################################
####################  Finding min/max ID ################################################
#########################################################################################

# Given: a df with politician timelines, and a date from which to start collecting posts,
# identify bounding tweet IDs for that date.
# that means, the minimum tweet ID for the next day (this is called the maximum bounding tweet ID), 
# and the maximum tweet ID for the previous day (this is called the minimum bounding tweet ID).  
# Return a list with: new polDF, containing also padded tweet IDs,
# the minimum bounding tweet ID, and the maximum bounding tweet ID.
# The min and max tweet IDs are used to bound incoming tweet IDs by date 
# (i.e to make sure that tweets for the given day are returned)

# default start ID is stored in a file in PoliticianList/TwStartID.RDS,
# and is set to correspond to September 8, 2019
get_BoundaryID <- function(useDate, polTL = NULL, startID = NULL, includeDF = TRUE) {
 
  if (is.null(startID)) {
    StartIDFile <- "PoliticianList/TwStartID.RDS"
    if (!file.exists(StartIDFile)) StartIDFile <- file.path("..", "Collect Comments", StartIDFile)
    startID <- readRDS(StartIDFile)
  }
  
  if (is.null(polTL)) polTL <- get_PolTimeline()
  
  if ("status_id" %in% names(polTL)) {
    maxLen <- max(nchar(polTL$status_id), 0, na.rm = TRUE)
    newDF <- mutate(polTL, padded_status_id = pad_id(status_id, maxLen))
    
    dfPrevDay <- filter(newDF, created_at < useDate)
    dfNextDay <- filter(newDF, created_at > useDate + 1)
  } else {
    maxLen <- max(nchar(polTL$id), 0, na.rm = TRUE)
    newDF <- mutate(polTL, padded_status_id = pad_id(id, maxLen)) 
    
    dfPrevDay <- filter(newDF, dateCreated < useDate)
    dfNextDay <- filter(newDF, dateCreated > useDate + 1)
  }
  
   
  minID <- max(dfPrevDay$padded_status_id, startID, na.rm = TRUE)  # the max ID of useDate - 1, 
  # but at any rate not less than April 15, 2019
  
  
  if (is_empty(dfNextDay) || nrow(dfNextDay) == 0L) {
    maxID <- NULL
  } else {
    maxID <- min(dfNextDay$padded_status_id, na.rm = TRUE)     # the min ID of useDate + 1
  }
  
  if (includeDF == TRUE) {
    result <- list(newDF = newDF, minID = minID, maxID = maxID)
  } else {
    result <- list(minID = minID, maxID = maxID)
  }
  
  return(result)
}





#########################################################################################
####################  JOINING Files #####################################################
#########################################################################################

# Given two tweet df's, dfOld and dfNew, determine which elements of dfOld are duplicated in dfNew
# For this function, duplicated means the tweet ID is the same, and, when the df's contain
# tweets for more than one politician, the politician to whom it is being sent is the same.
# Return the df consisting of the binding by row of dfOld and dfNew, removing first the rows
# in dfOld that are duplicated in dfNew
# use userID and not user (name) to compare, because some users have more than one screen name!!
# if checkSampled is TRUE, check which elements in newDF were already in the oldDF, and had Sampled = "Y"

bind_dfs <- function(dfOld, dfNew, onePol = FALSE, checkSampled = FALSE) {
   
  if (is_empty(dfOld) || nrow(dfOld) == 0) return(dfNew)
  
  print("in bind_dfs")

  isDupID <- dfOld$id %in% dfNew$id

  useDF <- filter(dfOld, !isDupID)

  if (onePol == TRUE) {  # if we are dealing with DFs of only one politician, we are done, we can just bind the old and new DFs
    dfOld2 <- useDF
  } else {
    dupOld <- filter(dfOld, isDupID)
    if (is_empty(dupOld) || nrow(dupOld) == 0) {
      dfOld2 <- dfOld
    } else {
      notDuplicate <- vapply(1:nrow(dupOld), function(i) {  # figure out which of the new comments are not duplicates of the old DF
        notDup <- TRUE
        dfNew2 <- filter(dfNew, id == dupOld$id[i])
        if (!is_empty(dfNew2) && nrow(dfNew2) > 0L) {
          BUsers <- dfNew2$replyToID  # who is the comment being sent in reply to?
          
          if (dupOld$replyToID[i] %in% BUsers) {
            notDup <- FALSE
          }
        } 
        return(notDup)
      }, FUN.VALUE = FALSE)
      dfOld2 <- filter(dupOld, notDuplicate) %>%
        bind_rows(useDF)
    }
  }
  newDF <- bind_rows(dfOld2, dfNew) %>%
    unique()
  
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


# Given two tweet df's, dfA and dfB, determine which elements of dfA are duplicated in dfB
# For this function, duplicated means the tweet ID is the same, and, when the df's contain
# tweets for more than one politician, the politician to whom it is being sent is the same.
# Return the df consisting of the duplicated elements
intersect_dfs <- function(dfA, dfB, onePol = TRUE) {
  
  if (is_empty(dfA) || nrow(dfA) == 0) return(NULL)
  
  isDupID <- dfA$id %in% dfB$id
  useA <- filter(dfA, isDupID)
  if (onePol == TRUE) {
    newDF <- useA
  } else {
    dupOld <- filter(dfA, isDupID)
    if (is_empty(dupOld) || nrow(dupOld) == 0) {
      newDF <- NULL
    } else {
      isDuplicate <- vapply(1:nrow(dupOld), function(i) {
        isDup <- FALSE
        newDFB <- filter(dfB, id == dupOld$id[i])
        if (!is_empty(newDFB) && nrow(newDFB) > 0L) {
          BUsers <- newDFB$replyToID
          if (dupOld$replyToID[i] %in% BUsers) {
            isDup <- TRUE
          }
        } 
        
        return(isDup)
      }, FUN.VALUE = FALSE)
      newDF <- filter(dupOld, isDuplicate)
    }
  }
  return(newDF)
}

archive_samplingFiles <- function(fName) {
  oldPath <- file.path("Data", "forSampling", fName)
  res <- FALSE
  if (file.exists(oldPath)) {
    newPath <- file.path("Data", "forSampling", "Packaged", fName)
    res <- file.copy(from = oldPath, to = newPath)
    if (res == TRUE) file.remove(oldPath)
  }
  return(res)
}


