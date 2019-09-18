
# DATE CREATED: 24.04.2019
# This file contains various utility functions for the sampling of FB Posts/Comments


#########################################################################################
######################  Count Politician Posts ##########################################
#########################################################################################

count_FBPosts <- function(polTL = NULL, untilDate = NULL) {
  if (is.null(polTL)) polTL <- get_PolFeed(returnRes = "Feed")
  
  if (!is_empty(untilDate)) {
    polTL <- filter(polTL, as.Date(dateCreated) <= untilDate)
  }
  
  if (is_empty(polTL) || nrow(polTL) == 0L) return(NULL)
  
  counts <- group_by(polTL, Politician) %>%
    summarise(numFBPosts = n())
}


count_FBPolLevels_ <- function(polLvl = NULL) {
  
  if (is.null(polLvl)) polLvl <- get_DownloadedComments(comOnly = TRUE)
  
  countLevels <- ddply(polLvl, "Politician", function(df) {
    
    if (is_empty(df)) return(data.frame(FBLevel_1 = 0, FBLevel_2 = 0, FBAllLevels = 0))
    newDF <- tibble(FBLevel_1 = nrow(df), FBLevel_2 = sum(df$comments_count), FBAllLevels = FBLevel_1 + FBLevel_2) 
    return(newDF)
    
  }) %>%
    replace_na(list(FBLevel_1 = 0, FBLevel_2 = 0, FBAllLevels = 0))
  return(countLevels)
}


count_FBPolLevels <- function(untilDate = NULL) {
  startDate <- as.Date("2019-04-15")
  maxFileNum <- as.integer(untilDate - startDate) %/% 3 + 1
  countLevels <- NULL
  
  for (i in 1:maxFileNum) {
    fName <- paste0("FBComs", i, ".RData")
    fPath1 <- file.path("Data", "FBData", fName)
    fPath2 <- file.path("Data", "FBData", "ready4Sampling", fName)
    fPath3 <- file.path("Data", "FBData", "Sampled", fName)
    if (file.exists(fPath1)) {
      polLvl <- get_DownloadedComments(comOnly = TRUE, FN = i)
    } else if (file.exists(fPath2)) {
      polLvl <- get_DownloadedComments(comFile = "ready4Sampling/FBComs", comOnly = TRUE, FN = i)
    } else if (file.exists(fPath3)) {
      polLvl <- get_DownloadedComments(comFile = "Sampled/FBComs", comOnly = TRUE, FN = i)
    } else polLvl <- NULL
    
    if (!(is_empty(polLvl) || nrow(polLvl) == 0L)) {
      if (!is_empty(untilDate)) {
      polLvl <- filter(polLvl, as.Date(dateCreated) <= untilDate)
      }
        countLevels <- count_FBPolLevels_(polLvl) %>%
          bind_rows(countLevels)
    }
  }
  
  totalLevels <- ddply(countLevels, "Politician", function(df) {
    
    if (is_empty(df)) return(data.frame(FBLevel_1 = 0, FBLevel_2 = 0, FBAllLevels = 0))
    newDF <- tibble(FBLevel_1 = sum(df$FBLevel_1, na.rm = TRUE), 
                    FBLevel_2 = sum(df$FBLevel_2, na.rm = TRUE), 
                    FBAllLevels = FBLevel_1 + FBLevel_2) 
    return(newDF)
    
  }) %>%
    replace_na(list(FBLevel_1 = 0, FBLevel_2 = 0, FBAllLevels = 0))
  
  return(totalLevels)
}


count_AllFBComments <- function(untilDate = NULL, polTL = NULL, polLvl = NULL, polDF = NULL) {
  if (is.null(polDF)) polDF <- get_AllPolDF("FB") 
  
  postCount <- count_FBPosts(polTL = polTL, untilDate = untilDate) 
  
  polLvlCount <- count_FBPolLevels(untilDate = untilDate) 
  
  countsDF <- left_join(postCount, polLvlCount, by = "Politician") %>%
    replace_na(list(FBLevel_1 = 0, FBLevel_2 = 0, FBAllLevels = 0)) %>%
    right_join(polDF, by = c("Politician" = "listName")) %>%
    select(Name, everything(), -Politician, -polHandles) %>%
    replace_na(list(numFBPosts = 0, FBLevel_1 = 0, FBLevel_2 = 0, FBAllLevels = 0)) %>%
    mutate(FBPostCom = numFBPosts + FBAllLevels)
  
  return(countsDF)
}
