

# DATE: 05.11.2018

# Sampling Twitter Comments
# Oct. 31 - Nov. 25 in first sampled Comments.



library(plyr)
library(tidyverse)
library(stringr)
library(stringi) 
library(rdrop2)



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
# - isFake (TRUE / FALSE) 


#########################################################################################
#####################  Prepare Packages from Sampled Comments ###########################
#########################################################################################

source("R Code/TwitterCode/TwitterUtilityFuncs.R")
source("R Code/FBCode/FaceBookUtilityFuncs.R")



make_Packages <- function(sPols, useDir, packNum, whichRound, NperPack = 100, numReplicates = 3) {
  
  j <- packNum
  samplePolDF <- bind_rows(sPols, .id = "PolName")
  inSample <- filter(samplePolDF, Include == "Y")
  N <- nrow(inSample)
  
  remainderPack <- (N %% NperPack) # N mod 50
  if ( remainderPack > 10) {
    if (remainderPack > 0.75*NperPack) {
      numPacks <- (N %/% NperPack) + 1  # N div 100 (integer division) + 1
      plusItems <- 0
      lastPack <- remainderPack
    } else {
      numPacks <- max((N %/% NperPack), 1)
      plusItems <- remainderPack %/% numPacks
      lastPack <- (remainderPack %% numPacks) + plusItems + NperPack*min((N %/% NperPack), 1)
    }
  } else {
    numPacks <- max((N %/% NperPack), 1)
    plusItems <- 0
    lastPack <- NperPack*min((N %/% NperPack), 1) + remainderPack
  }
  
  numItems <- c(rep(NperPack + plusItems, numPacks - 1), lastPack)
  print(paste0("The addition is correct: ", sum(numItems) == N))
  
  set.seed(100)
  useRows <- sample.int(N, N)
  inSample <- inSample[useRows, ]  # permute the rows
  
  startR <- 1
  for (k in 1:numPacks) {
    useItems <- numItems[k]
    endR <- startR + useItems - 1
    useSample <- inSample[startR:endR, ]
    
    useReplyID <- useSample$replyToID
    addRows <- filter(samplePolDF, id %in% useReplyID, !(id %in% useSample$id)) %>%
      mutate(Include = "N")
    
    finalSampleDF <- bind_rows(useSample, addRows) %>%
      unique()
    
    packageList <- split(finalSampleDF, finalSampleDF$PolName) %>%
      llply(select, -PolName)
    
    PoliticianComments <- packageList
    
    
    for (l in 1:numReplicates) {
      if (nchar(j) < 2) padJ <- paste0("0", j) else padJ <- j
      if (nchar(k) < 2) padK <- paste0("0", k) else padK <- k
      fName <- paste0(whichRound, padJ, padK, "_PSPComments.RData")
      newDir <- paste(useDir, l, sep = "_")
      fPath <- file.path(newDir, fName)
      save(PoliticianComments, file = fPath)
    }
    startR <- endR + 1
  }
  
  return(NULL)
}

# get the file with the PolComments
# Lets take 3 politicians at a time, and create packages containing approx <NperPack> comments:
set.seed(495)


# which sample are we looking for: (odd = Twitter, even = FB)
sampleNum <- get_fileNum(fPattern = "PolComments", nPattern = "\\d?[02468]", decr = FALSE) 


fPath <- paste0("Data/forSampling/PolComments", sampleNum, ".RData")
attach(fPath)
AllPolComments <- AllPolComments

Round <- 1000 + 10*(sampleNum-1)


prevTimes <- numTimes + 1
detach(paste0("file:", fPath), character.only = TRUE)


numPols <- length(AllPolComments)
totTimes <- (numPols %/% 3) - 1
lastNum <- (numPols %% 3) + 3
numReps <- 2    # determine the number of replicates to create of each batch.  For Round 100 it was 3.
                       # Rounds 101 and 102 will be 2.

PSPDir <- paste0("Packages/New4PSP_R", Round)
for (l in 1:numReps) {
  newDir <- paste(PSPDir, l, sep = "_")
  if (!dir.exists(newDir)) {
    dir.create(newDir)
  }
}

numTimes <- totTimes

for (j in prevTimes:numTimes) {
  
  s1 <- (j - 1)*3 + 1
  s2 <- j*3
  samplePols <- AllPolComments[s1:s2]
  names(samplePols)
  make_Packages(samplePols, useDir = PSPDir, packNum = j, whichRound = Round, NperPack = 80, numReplicates = numReps)
 }


# if numTimes = totTimes, tack on the last package
if (numTimes == totTimes) {
  s1 <- (numTimes)*3 + 1
  s2 <- numPols
  samplePols <- AllPolComments[s1:s2]
  names(samplePols)
  numTimes <- numTimes + 1
  make_Packages(samplePols, useDir = PSPDir, packNum = numTimes, whichRound = Round, NperPack = 80, numReplicates = numReps)
  
}


# Quick package check:
useDir <- "Packages/New4PSP_R1150_1"
packs <- dir(useDir)

errLog <- list()

for(f in packs) {
  fPath <- file.path(useDir, f)
  load(fPath)
  l <- length(PoliticianComments)
  # quick check that all replyToIDs are contained in the files
  for (i in 1:l) {
    temp <- PoliticianComments[[i]]
    temp2 <- filter(temp, Include == "Y", Level > 0L)
    if (!all(temp2$replyToID %in% temp$id)) {
      print(paste0("In file ", f, " list element ", i, " there are some missing replies."))
      errLog[[f]] <- tibble(File = f, Item = i)
    }
  }
}


fName <- paste0("Backup/", as.integer(Sys.time()), "_PolComments", sampleNum, ".RData")
save(AllPolComments, Round, numTimes, file = fName)

fName <- paste0("PolComments", sampleNum, ".RData")
save(AllPolComments, Round, numTimes, file = file.path("Data", "forSampling", fName))


archive_samplingFiles(fName)




