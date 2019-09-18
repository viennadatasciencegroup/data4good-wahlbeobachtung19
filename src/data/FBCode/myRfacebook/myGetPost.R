
# Message from FB: 13.09.2019
# (#12) type field is deprecated for versions v3.3 and higher

myGetPost <- function(post, token, minDate, n=500, comments=TRUE, likes=(!reactions), reactions=FALSE, n.likes=n,
                    n.comments=n, n.reactions=n, api=NULL){
  
  
  print("In myGetPost")
  url <- paste0("https://graph.facebook.com/", post,
                "?fields=from,message,created_time,link,name,shares")
  
  if (comments==TRUE){
    url <- paste0(url, ",comments.summary(true).",
                  "fields(id,from,message,created_time,like_count,comment_count)")
    if (n.comments>=500){
      url <- paste0(url, ".limit(500)")
    }
    if (n.comments<500){
      url <- paste0(url, ".limit(", n.comments, ")")
    }
  }
  if (comments==FALSE){
    url <- paste0(url, ",comments.summary(true)")
  }
  if (likes==TRUE){
    url <- paste0(url, ",likes.summary(true)")
    # url <- paste0(url, ",likes.summary(true).",
    #               "fields(id,name)")
    if (n.likes>=2000){
      url <- paste0(url, ".limit(2000)")
    }
    if (n.likes<2000){
      url <- paste0(url, ".limit(", n.likes, ")")
    }
  }
  if (likes==FALSE){
    url <- paste0(url, ",likes.summary(true)")
  }
  if (reactions==TRUE){
    url <- paste0(url, ",reactions.summary(true)")
    # url <- paste0(url, ",reactions.summary(true).",
    #               "fields(id,type,name)")
    if (n.reactions>=2000){
      url <- paste0(url, ".limit(2000)")
    }
    if (n.reactions<2000){
      url <- paste0(url, ".limit(", n.reactions, ")")
    }
  }
  
  # making query
  content <- callAPI(url=url, token=token, api=api)

  # error traps: retry 3 times if error
  error <- 0
  while (length(content$error_code)>0){
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPI(url=url, token=token)		
    if (error==3){ stop(content$error_msg) }
  }
  if (length(content)==0){ 
    stop("Post could not be found")
  }
  
  # putting it together
  out <- list()
  out[["post"]] <- myPostDataToDF(content)
  if (likes && n.likes > 0) out[["likes"]] <- likesDataToDF(content$likes$data)
  if (likes && n.likes > 0) n.l <- ifelse(!is.null(out$likes), dim(out$likes)[1], 0)
  if (n.likes == 0) n.l <- 0
  if (!likes) n.l <- Inf
  if (comments && n.comments > 0) out[["comments"]] <- commentsDataToDF(content$comments$data)
  if (comments && n.comments > 0) n.c <- ifelse(!is.null(out$comments), dim(out$comments)[1], 0)
  if (n.comments == 0) n.c <- 0
  if (!comments) n.c <- Inf
  if (reactions && n.reactions > 0) out[["reactions"]] <- reactionsDataToDF(content$reactions$data)
  if (reactions && n.reactions > 0)  n.r <- ifelse(!is.null(out$reactions), dim(out$reactions)[1], 0)
  if (n.reactions == 0) n.r <- 0
  if (!reactions) n.r <- Inf
  
  
  # paging if n.comments OR n.likes haven't been downloaded
  if (n.likes > n.l || n.comments > n.c || n.reactions > n.r){
    # saving URLs for next batch of likes and comments
    if (likes) url.likes <- content$likes$paging$`next`
    if (!likes) url.likes <- NULL
    if (comments) url.comments <- content$comments$paging$`next`
    if (!comments) url.comments <- NULL
    if (reactions) url.reactions <- content$reactions$paging$`next`
    if (!reactions) url.reactions <- NULL
    
    if (!is.null(url.likes) && likes && n.l > 0 && n.likes > n.l){
      # retrieving next batch of likes
      url <- content$likes$paging$`next`
      content <- callAPI(url=url.likes, token=token, api=api)
      out[["likes"]] <- rbind(out[["likes"]],
                              likesDataToDF(content$data))
      n.l <- dim(out$likes)[1]
      # next likes, in batches of 500
      while (n.l < n.likes & length(content$data)>0 &
             !is.null(url <- content$paging$`next`)){
        url <- content$paging$`next`
        content <- callAPI(url=url, token=token)
        out[["likes"]] <- rbind(out[["likes"]],
                                likesDataToDF(content$data))
        n.l <- dim(out$likes)[1]
      }
    }
    
    if (!is.null(url.reactions) && reactions && n.r > 0 && n.reactions > n.r){
      # retrieving next batch of reactions
      url <- content$reactions$paging$`next`
      content <- callAPI(url=url.reactions, token=token, api=api)
      out[["reactions"]] <- rbind(out[["reactions"]],
                                  reactionsDataToDF(content$data))
      n.r <- dim(out$reactions)[1]
      # next reactions, in batches of 500
      while (length(content$data)>0 && n.r < n.reactions &
             !is.null(url <- content$paging$`next`)){
        url <- content$paging$`next`
        content <- callAPI(url=url, token=token, api=api)
        out[["reactions"]] <- rbind(out[["reactions"]],
                                    reactionsDataToDF(content$data))
        n.r <- dim(out$reactions)[1]
      }
    }
    
    
    temp <- out[["comments"]]
    TimeStamp <- as.integer(Sys.time())
    save(temp, file = paste0("Data/Temp/", TimeStamp, "_MissingComments.RData"))
    
    if (is_empty(temp) || nrow(temp) == 0L) upToDate <- TRUE else upToDate <- min(as.Date(temp$created_time)) < minDate
    
    if (!is.null(url.comments) && comments && !upToDate && n.c > 0 && n.comments > n.c){
      # retrieving next batch of comments -- taking rests in-between
      Sys.sleep(1)
      content <- callAPI(url=url.comments, token=token, api=api)
      out[["comments"]] <- rbind(out[["comments"]],
                                 commentsDataToDF(content$data))
      temp <- out[["comments"]]
      save(temp, file = paste0("Data/Temp/", TimeStamp, "_MissingComments.RData"))
      
      n.c <- dim(out$comments)[1]
      upToDate <- min(as.Date(temp$created_time)) < minDate
      # next comments, in batches of 500
      numRounds <- 2  # count how many times we call the API, so we know how long to make the calls rest
      while (n.c < n.comments & length(content$data)>0 & !upToDate &
             !is.null(content$paging$`next`)  & numRounds < 49){  # FB does not allow more than 49 rounds!
        numRounds <- numRounds + 1
        
        if (numRounds %% 20 == 0) {
          TimeStamp <- as.integer(Sys.time())
          sleepTime <- 60
        } else sleepTime <- 1
        
        print(paste0("in myGetPost, numRounds is: ", numRounds, " and sleepTime is: ", sleepTime))
        
        Sys.sleep(sleepTime)   # rest in-between calls
        
        url <- content$paging$`next`
        content <- callAPI(url=url, token=token, api=api)
        out[["comments"]] <- rbind(out[["comments"]],
                                   commentsDataToDF(content$data))
        
        temp <- out[["comments"]]
        save(temp, file = paste0("Data/TempC/", TimeStamp, "_MissingFBComments.RData"))
        
        n.c <- dim(out$comments)[1]
        upToDate <- min(as.Date(temp$created_time)) < minDate
      }
    }
  }
  
  return(out)
}



