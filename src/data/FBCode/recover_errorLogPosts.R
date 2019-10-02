

# DATE: 18.05.2019

## Code for recovering Posts/Comments when errors occur

#########################################################################################
####################### Fix errorLog Posts ##############################################
#########################################################################################

errors <- bind_rows(errorLog)

errorPolDF <- filter(polDF, polHandles %in% errors$Pol)


for (i in 1:nrow(errorPolDF)) {
  pol <- errorPolDF$listName[i]
  print(pol)
  pHandle <- errorPolDF$polHandles[i]
  resp <- tryCatch(get_FBPosts(pHandle,
                               sinceDate = sinceDate - 2,
                               untilDate = untilDate - 2,
                               num = 2000,
                               FBToken = FBToken),
                   error = function(e) {
                     print(e)
                     msg <- as.character(simpleError(e))
                     errorInfo <- list(Pol = pol, Msg = msg, Time = Sys.time())
                     errorLog[[pHandle]] <<- errorInfo
                     Sys.sleep(10)
                     return(NULL)
                   })
  FBFeedList[[pol]] <- resp
  
  if (i %% 5 == 0L) Sys.sleep(300) else Sys.sleep(60)
}




#########################################################################################
####################### Fix errorLogC posts #############################################
#########################################################################################

errors <- bind_rows(errorLogC)

deletedPosts <- errors %>%
  filter(str_detect(Msg, "Unsupported get request")) %>%
  pull(Post)

errorPosts <- errors %>%
  filter(!str_detect(Msg, "Unsupported get request")) %>%
  pull(Post)

fixIndex <- which(StillToDownload$id %in% errorPosts)
totalComments <- 0
for (s in fixIndex) {
  
  pol <- StillToDownload$Politician[s]
  replyTo <- StillToDownload$user[s]
  Lvl <- StillToDownload$Level[s]
  print(paste0("inside all_fb_comments, Politician, s is: ", pol, ", ", s))
  numComments <- StillToDownload$comments_count[s]
  totalComments <- totalComments + numComments
  
  if (numComments > 0) {
    maxDay <- min(as.Date(StillToDownload$dateCreated[s]) + 11, maxDate)
    res <- tryCatch(get_FBComments(StillToDownload$id[s], 
                                   sinceDate = StillToDownload$CommentsAvail[s],
                                   untilDate = maxDay,
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
    if (totalComments %% 20000 == 0L) Sys.sleep(200) else Sys.sleep(5)
  }
  
  
}

errorLogC <- NULL





#########################################################################################
####################### Fix errorLogR comments ##########################################
#########################################################################################

errDF <- filter(errors, !str_detect(Msg, "Unsupported get request"))
fixIndex <- which(StillToDownload$id %in% errDF$Comment)
FBToken <- load_Token(3)
totalComments <- 0

for (i in fixIndex) {
  print(i)
  out <- StillToDownload[i, ]
  numComments <- out$comments_count
  if (numComments > 0) {
    totalComments <- totalComments + numComments
    origPost <- out$origPost
    print(paste0("inside all_replies, original Post is ", origPost, " and i is: ", i))
    
    res <- tryCatch(get_FBReplies(out, 
                                  numRep = 10000,    # indicate a maximum number of replies to get
                                  untilDate = as.Date(out$dateCreated) + 7),   
                    error = function(e) {
                      msg <- as.character(simpleError(e))
                      print(msg)
                      return(NULL)
                    })
    if (!is_empty(res)) {
      comReps <- bind_FBdfs(comReps, res)
      StillToDownload$repAvail[i] <- Sys.Date()
      
      if (i %% 10 == 0L) {
        save(comReps, file = "Data/FBData/TempR/Replies.RData")
        Sys.sleep(10)
      }
    } 
    
  }
  
  Sys.sleep(10)
  print("************************************")
  
}

if (!is.null(comReps)) {
  comReps <- filter(comReps, !(text %in% c("", " "))) %>%  # get rid of empty messages
    replace_na(list(comments_count = 0))
  save(comReps, file = "Data/FBData/TempR/Replies4.RData")
}

errorLogR <- NULL









