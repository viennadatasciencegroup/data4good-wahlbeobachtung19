
library(Rfacebook)

myReplyDataToDF <- function(json) 
{
  df <- data.frame(from_id = ifelse(!is.null(json$from$id), json$from$id, NA), 
                   from_name = ifelse(!is.null(json$from$name), json$from$name, NA),
                   message = ifelse(!is.null(json$message), json$message, 
                                    NA), 
                   created_time = json$created_time, likes_count = json$like_count, 
                   comments_count = json$comment_count, id = json$id, stringsAsFactors = F)
  return(df)
}


myRepliesDataToDF <- function(json) 
{
  if (!is.null(json)) {
    df <- data.frame(from_id = unlistWithNA(json, c("from", 
                                                    "id")), from_name = unlistWithNA(json, c("from", 
                                                                                             "name")), message = unlistWithNA(json, "message"), 
                     created_time = unlistWithNA(json, "created_time"), 
                     likes_count = unlistWithNA(json, "like_count"), id = unlistWithNA(json, 
                                                                                       "id"), stringsAsFactors = F)
  }
  if (is.null(json)) {
    df <- NULL
  }
  return(df)
}


myGetCommentReplies <- function(comment_id, token, n = 500, replies = TRUE, likes = FALSE, 
          n.likes = n, n.replies = n, api = NULL) 
{
  url <- paste0("https://graph.facebook.com/", comment_id, 
                "?fields=from,message,created_time,like_count,comment_count")
  if (replies == TRUE) {
    url <- paste0(url, ",comments.summary(true).", "fields(from,id,message,created_time,like_count)")
    if (n.replies >= 500) {
      url <- paste0(url, ".limit(500)")
    }
    if (n.replies < 500) {
      url <- paste0(url, ".limit(", n.replies, ")")
    }
  }
  if (replies == FALSE) {
    url <- paste0(url, ",comments.summary(true)")
  }
  if (likes == TRUE) {
    url <- paste0(url, ",likes.summary(true).", "fields(id,name)")
    if (n.likes >= 2000) {
      url <- paste0(url, ".limit(2000)")
    }
    if (n.likes < 2000) {
      url <- paste0(url, ".limit(", n.likes, ")")
    }
  }
  if (likes == FALSE) {
    url <- paste0(url, ",likes.summary(true)")
  }
  content <- callAPI(url = url, token = token)
  error <- 0
  while (length(content$error_code) > 0) {
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPI(url = url, token = token, api = api)
    if (error == 3) {
      stop(content$error_msg)
    }
  }
  if (length(content) == 0) {
    message("Comment could not be found")
    return(data.frame())
  }
  out <- list()
  out[["comment"]] <- myReplyDataToDF(content)
  if (likes && n.likes > 0) 
    out[["likes"]] <- likesDataToDF(content$likes$data)
  if (likes && n.likes > 0) 
    n.l <- ifelse(!is.null(out$likes), dim(out$likes)[1], 
                  0)
  if (n.likes == 0) 
    n.l <- 0
  if (!likes) 
    n.l <- Inf
  if (replies && n.likes > 0) 
    out[["replies"]] <- myRepliesDataToDF(content$comments$data)
  if (replies && n.likes > 0) 
    n.c <- ifelse(!is.null(out$replies), dim(out$replies)[1], 
                  0)
  if (n.replies == 0) 
    n.c <- 0
  if (!replies) 
    n.c <- Inf
  if (n.likes > n.l || n.replies > n.c) {
    if (likes) 
      url.likes <- content$likes$paging$`next`
    if (!likes) 
      url.likes <- NULL
    if (replies) 
      url.comments <- content$comments$paging$`next`
    if (!replies) 
      url.comments <- NULL
    if (!is.null(url.likes) && likes && n.likes > n.l) {
      url <- content$likes$paging$`next`
      content <- callAPI(url = url.likes, token = token, 
                         api = api)
      out[["likes"]] <- rbind(out[["likes"]], likesDataToDF(content$data))
      n.l <- dim(out$likes)[1]
      while (n.l < n.likes & length(content$data) > 0 & 
             !is.null(url <- content$paging$`next`)) {
        url <- content$paging$`next`
        content <- callAPI(url = url, token = token, 
                           api = api)
        out[["likes"]] <- rbind(out[["likes"]], likesDataToDF(content$data))
        n.l <- dim(out$likes)[1]
      }
    }
    if (!is.null(url.comments) && replies && n.replies > 
        n.c) {
      content <- callAPI(url = url.comments, token = token, 
                         api = api)
      out[["replies"]] <- rbind(out[["replies"]], myRepliesDataToDF(content$data))
      n.c <- dim(out$replies)[1]
      while (n.c < n.replies & length(content$data) > 0 & 
             !is.null(content$paging$`next`)) {
        url <- content$paging$`next`
        content <- callAPI(url = url, token = token, 
                           api = api)
        out[["replies"]] <- rbind(out[["replies"]], myRepliesDataToDF(content$data))
        n.c <- dim(out$replies)[1]
      }
    }
  }
  return(out)
}


