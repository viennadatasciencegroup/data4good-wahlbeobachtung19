
mySearchPages <- function(string, token, n=200)
{
  # disable http/2, it is causing problems!!!
  httr::set_config(httr::config(http_version = 0))
  
  if (length(string)>1){ string <- paste(string, collapse=" ") }
  
  url <- paste("https://graph.facebook.com/pages/search?q=", string,
               "&limit=", sep="")
  if (n<=200){
    url <- paste(url, n, sep="")
  }
  if (n>200){
    url <- paste(url, "200", sep="")
  }
  url <- paste(url, 
               "&fields=id,name,verification_status,link,location,is_unclaimed",
               sep="")
  
  url <- utils::URLencode(url)
  
  ## making query
  content <- callAPI(url=url, token=token)
  l <- length(content$data); cat(l, "pages ")
  
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
    stop("No public page mentioning the string were found")
  }
  df <- searchPageDataToDF(content$data)
  
  ## paging if n>200
  if (n>200){
    df.list <- list(df)
    while (l<n & length(content$data)>0 &
           !is.null(content$paging$`next`)){
      url <- content$paging$`next`
      content <- callAPI(url=url, token=token)
      l <- l + length(content$data)
      if (length(content$data)>0){ cat(l, " ") }
      
      ## retrying 3 times if error was found
      error <- 0
      while (length(content$error_code)>0){
        cat("Error!\n")
        Sys.sleep(0.5)
        error <- error + 1
        content <- callAPI(url=url, token=token)		
        if (error==3){ stop(content$error_msg) }
      }
      
      df.list <- c(df.list, list(searchPageDataToDF(content$data)))
    }
    df <- do.call(rbind, df.list)
  }
  return(df)
}