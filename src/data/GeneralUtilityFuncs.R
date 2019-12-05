
# DATE CREATED: 24.04.2019
# This file contains various utility functions for the collection, saving, and sampling of all comments



#########################################################################################
#######################     Global Variables    #########################################
#########################################################################################

firstDay <- as.Date("2019-09-08")
lastPostDay <- as.Date("2019-09-30")
lastDay <- as.Date("2019-10-04")




#########################################################################################
#######################     Clear Directories    ########################################
#########################################################################################

clean_dropDirectory <- function(dName) {
  ## This function removes all files from a given DropBox directory ##
  closeAllConnections()
  
  
  if (drop_exists(dName)) {
    dFiles <- drop_dir(dName)
    
    if (!is_empty(dFiles)) {   
      deleteFiles <- dFiles$path_display
      keepFile <- str_detect(deleteFiles, "AmnestyEvalSummary.RData")
      deleteFiles <- deleteFiles[!keepFile]
      
      if (!is_empty(deleteFiles)) {
        lapply(deleteFiles, drop_delete)
      }
    }
    
  }
  closeAllConnections()
  return(invisible())
}



clean_Directory <- function(dName) {
  ## Remove all files from a given local directory. ##
  
  if (file.exists(dName)) {
    dFiles <- list.files(dName)
    if (!is_empty(dFiles)) {
      deleteFiles <- file.path(dName, dFiles)
      lapply(deleteFiles, file.remove)
    }
  } else dir.create(dName)
  return(invisible())
}


#########################################################################################
####################  Getting all Politician Names ######################################
#########################################################################################

## Call prep_PolDF once at the beginning of data collection
# to clean up the CSV file before working with it

prep_PolDF <- function(fName = "List_of_Politicians.csv") {
  polFile <- file.path("PoliticianList", "List_of_Politicians_Prelim.csv")
  polDF <- read_csv(file = polFile)
  
  polDF <- mutate(polDF, FB_Confirmed = str_replace(FACEBOOK, "https://de-de.facebook.com", "https://www.facebook.com/")) %>%
    drop_na(Nachname)
  
  print(sum(is.na(polDF$Nachname)))
  
  polDF <- polDF %>% 
    mutate(Name = str_to_upper(str_c(polDF$Vorname, polDF$Nachname, sep = " "))) %>%
    mutate(Name = ifelse(is.na(Name), str_to_upper(Nachname), Name) %>%
             str_replace("\\(.+\\)", "") %>%
             str_replace_all("\\?", "") %>%
             str_squish())
  
  polDF <- mutate(polDF, Twitter_Confirmed = TWITTER) %>%
    drop_na(Name)
  
  print(sum(is.na(polDF$Name)))
  
  polFile <- file.path("PoliticianList", fName)
  write_csv(polDF, polFile)
  
  return(polDF)
}


# Returns a data frame with politician names and their FB/twitter handles (depending on "Site")
# Site can be "Twitter" or "FB"
get_AllPolDF <- function(Site = "Twitter", fName = NULL) {
  
  if (Site %in% c("Twitter", "Tw")) returnDF <- get_TwPolDF(fName) else returnDF <- get_FBPolDF(fName)
  
  return(returnDF)
}

# Returns DF with Politician Info: Lista, Partita, Gender, Circoscrizione
get_AllPolInfo <- function() {
  polFile <- file.path("PoliticianList", "List_of_Politicians.csv")
  if (!file.exists(polFile)) polFile <- file.path("..", "Collect Comments", polFile)
  
  infoDF <- read_csv(file = polFile) %>%
    select(Type, Party, Funktion, Name)
  
  
  lName <- str_replace_all(infoDF$Name, " ", "_") %>%
    str_replace_all("Ä", "AE") %>%
    str_replace_all("Ö", "OE") %>%
    str_replace_all("Ü", "UE") %>%
    str_replace_all("[^A-z0-9_]", "")
  
  returnDF <- mutate(infoDF, listName = lName) %>%
    select(Name, listName, everything()) %>%
    unique()
  
  return(returnDF)
}


# Given the FB/Twitter handle, find the politician name
get_PolName <- function(Handle, polDF = NULL, Site = "Twitter") {
  if (is.null(polDF)) polDF <- get_AllPolDF(Site)
  if (Site == "Twitter") pName <- get_TwPolName(Handle, polDF = polDF) else pName <- get_FBPolName(Handle, polDF = polDF)
  return(pName)
}


# Given the FB/Twitter handle, find the list name (pol name without special characters)
get_listName <- function(Handle, polDF = NULL, Site = "Twitter") {
  if (is.null(polDF)) polDF <- get_AllPolDF(Site)
  if (Site == "Twitter") lName <- get_TwlistName(Handle, polDF = polDF) else lName <- get_FBlistName(Handle, polDF = polDF)
  return(lName)
}


# Given the politician name, or the list name, find the corresponding twitter handle
get_PolHandle <- function(pName, polDF = NULL, Site = "Twitter") {
  
  if (is.null(polDF)) polDF <- get_AllPolDF(Site)
  
  if (Site == "Twitter") pHandle <- get_TwPolHandle(pName, polDF = polDF) else pHandle <- get_FBPolHandle(pName, polDF = polDF) 
  return(pHandle)
}




#########################################################################################
##########################  Other General Utility Functions #############################
#########################################################################################

# Remove NULL elements from a list
removeNull <- function(L, isDF = FALSE) {
  # Remove NULL elements from the list L
  if (isDF == TRUE) {
    isNULL <- ldply(L, function(df) {
      res <- (is_empty(df) || nrow(df) == 0L)
    })$V1
  } else {
    isNULL <- ldply(L, is_empty, .id = NULL)$V1
  }
  newL <- L[!isNULL]
  return(newL)
}

# get the list of politicians being sampled from so far
get_UsedPols <- function(Site = "All") {
  fPath <- file.path("Data", "forSampling", "SampledPols.RData")
  if (file.exists(fPath)) {
    attach(fPath)
    TwPols <- TwPols
    FBPols <- FBPols
    detach(paste0("file:", fPath), character.only = TRUE)
  } else {
    TwPols <- FBPols <- NULL
  }
  if (Site == "Tw") return(TwPols) else if (Site == "FB") return(FBPols) else return(list(TwPols = TwPols, FBPols = FBPols))
}


# update the list of politicians being sampled from
update_usedPols <- function(pNames = NULL, Site = "Tw", maxPols = NULL) {
  numCom <- get_NumCom()
  followPols <- names(numCom)
  
  if (is_empty(pNames)) {
    samplePols <- followPols
  } else {
    samplePols <- followPols[followPols %in% pNames]
  }
  
  if (!is_empty(maxPols)) {
    samplePols <- samplePols[1:maxPols]
  }
  
  AllPols <- get_UsedPols("All")
  TwPols <- AllPols$TwPols
  FBPols <- AllPols$FBPols
  
  if (Site == "Tw") {
    TwPols <- unique(c(samplePols, TwPols))
    samplePols <- TwPols
  } else {
    FBPols <- unique(c(samplePols, FBPols))
    samplePols <- FBPols
  }
  
  fPath <- file.path("Data", "forSampling", "SampledPols.RData")
  save(TwPols, FBPols, file = fPath)
  
  return(samplePols)
}


# Find the fileNumber of the sampled comments or politician comments package
get_fileNum <- function(sDir = "Data/forSampling", fPattern = "SampledComments5\\d{2}", nPattern = "5\\d{2}", decr = TRUE, checkODD = NULL) {
  
  fileNum <- dir(sDir) %>%
    str_subset(fPattern) %>%
    str_extract(nPattern) %>%
    as.integer() %>%
    sort(decreasing = decr) %>%
    .[1] %>%
    max(0, na.rm = TRUE)
  
  return(fileNum)
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



