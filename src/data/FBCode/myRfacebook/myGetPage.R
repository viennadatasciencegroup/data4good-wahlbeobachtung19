
# Message from FB: 13.09.2019
# (#12) type & link field is deprecated for versions v3.3 and higher



# Modifying P. Barbera's getPage function to get access to public posts:

myGetPage <- function(page, token, n=25, since=NULL, until=NULL, feed=FALSE, reactions=FALSE, 
                    verbose=TRUE, api=NULL){
  
  # disable http/2, it is causing problems!!!  this is now done in callAPI
  # h <- curl::new_handle()
  # curl::handle_setopt(h, http_version = 2)
  
  url <- paste0('https://graph.facebook.com/', page,
                '/posts?fields=from,message,created_time,story,comments.summary(true)',
                ',likes.summary(true),shares')
  if (feed){
    url <- paste0('https://graph.facebook.com/', page,
                  '/feed?fields=from,message,created_time,story,comments.summary(true)',
                  ',likes.summary(true),shares')
  }
  if (!is.null(until)){
    url <- paste0(url, '&until=', until)
  }
  if (!is.null(since)){
    url <- paste0(url, '&since=', since)
  }
  if (n<=25){
    url <- paste0(url, "&limit=", n)
  }
  if (n>25){
    url <- paste0(url, "&limit=25")
  }
  # making query
  content <- callAPI(url=url, token=token, api=api)
  l <- length(content$data); if (verbose) cat(l, "posts ")
  
  print(paste0("Got first API call, length is: ", l))
  
  ## retrying 3 times if error was found
  error <- 0
  while (length(content$error_code)>0){
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPI(url=url, token=token)		
    if (error==3){ stop(content$error_msg) }
  }
  if (length(content$data)==0){ 
    message("No public posts were found : ", page)
    return(data.frame())
  }
  df <- pageDataToDF(content$data)
  
  # sometimes posts older than 'until' are returned, so here
  # I'm making sure the function stops when that happens
  if (!is.null(since)){
    dates <- formatFbDate(df$created_time, 'date')
    mindate <- min(dates)
    sincedate <- as.Date(since)
  }
  if (is.null(since)){
    sincedate <- as.Date('1970/01/01')
    mindate <- as.Date(Sys.time())
  }
  
  ## paging if n>25
  if (n>25){
    df.list <- list(df)
    while (l<n & length(content$data)>0 & 
           !is.null(content$paging$`next`) & sincedate <= mindate){
      # waiting 20 seconds before making next API call...
      Sys.sleep(20)
      
      url <- content$paging$`next`
      content <- callAPI(url=url, token=token, api=api)
      l <- l + length(content$data)
      if (length(content$data)>0){ if (verbose) cat(l, "posts ") }
      
      ## retrying 3 times if error was found
      error <- 0
      while (length(content$error_code)>0){
        cat("Error!\n")
        Sys.sleep(0.5)
        error <- error + 1
        content <- callAPI(url=url, token=token, api=api)
        if (error==3){ stop(content$error_msg) }
      }
      new.df <- pageDataToDF(content$data)
      df.list <- c(df.list, list(new.df))
      
      if (!is.null(since) & nrow(new.df)>0){
        dates <- formatFbDate(new.df$created_time, 'date')
        mindate <- min(dates)
      }
    }
    df <- do.call(rbind, df.list)
  }
  # returning only those requested
  if (nrow(df)>n){
    df <- df[1:n,]
  }
  
  # deleting posts after specified date
  if (!is.null(since)){
    dates <- formatFbDate(df$created_time, 'date')
    df <- df[dates>=sincedate,]
  }
  
  # adding reactions data
  if (reactions==TRUE){
    Sys.sleep(10)  # wait 10 seconds
    re = getReactions(df$id, token=token, verbose=FALSE, api=api)
    df <- merge(df, re, all.x=TRUE)
    # sorting
    df <- df[order(df$created_time),]
  }
  
  return(df)
}
