

# DATE: 18.05.2019

## Code for recovering Posts/Comments when errors occur

#########################################################################################
####################### Fix errorLog Posts ##############################################
#########################################################################################

errors <- bind_rows(errorLog)

errorPols <- filter(polDF, polHandles %in% errors$Pol)
polDF <- get_AllPolDF("FB")
errorPolDF <- filter(polDF, listName %in% errorPols$listName)

for (i in 1:nrow(errorPolDF)) {
  pol <- errorPolDF$listName[i]
  print(pol)
  pHandle <- errorPolDF$polHandles[i]
  resp <- tryCatch(get_FBPosts(pHandle,
                               sinceDate = sinceDate,
                               untilDate = untilDate,
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
  Sys.sleep(10)
}




#########################################################################################
####################### Fix errorLogC posts #############################################
#########################################################################################


errPosts <- names(errorLogC)

fixIndex <- which(StillToDownload$id %in% errPosts)
totalComments <- 0
for (s in fixIndex) {
  
  pol <- StillToDownload$Politician[s]
  replyTo <- StillToDownload$user[s]
  print(paste0("inside all_fb_comments, Politician, s is: ", pol, ", ", s))
  numComments <- StillToDownload$comments_count[s]
  totalComments <- totalComments + numComments
  
  if (numComments > 0) {
    lastDay <- min(as.Date(StillToDownload$dateCreated[s]) + 11, maxDate)
    res <- tryCatch(get_FBComments(StillToDownload$id[s], 
                                   sinceDate = StillToDownload$CommentsAvail[s],
                                   untilDate = lastDay,
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
      newComs <- bind_FBdfs(newComs, res)
      StillToDownload$CommentsAvail[s] <- Sys.Date() - 1
    }
    
    save(newComs, s, file = "Data/TempC/Comments.RData")
    if (totalComments %% 20000 == 0L) Sys.sleep(200) else Sys.sleep(5)
  }
  
  
}

errorLogC <- NULL








