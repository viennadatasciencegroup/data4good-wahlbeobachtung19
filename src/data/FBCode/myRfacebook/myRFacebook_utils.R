
searchDataToDF <- function(json){
  df <- data.frame(
    from_id = unlistWithNA(json, c('from', 'id')),
    from_name = unlistWithNA(json, c('from', 'name')),
    message = unlistWithNA(json, 'message'),
    created_time = unlistWithNA(json, 'created_time'),
    type = unlistWithNA(json, 'type'),
    link = unlistWithNA(json, 'link'),
    id = unlistWithNA(json, 'id'),
    likes_count = unlistWithNA(json, c('likes', 'summary', 'total_count')),
    comments_count = unlistWithNA(json, c('comments', 'summary', 'total_count')),
    shares_count = unlistWithNA(json, c('shares', 'count')),
    stringsAsFactors=F)
  return(df)
}

newsDataToDF <- function(json){
  df <- data.frame(
    from_id = unlistWithNA(json, c('from', 'id')),
    from_name = unlistWithNA(json, c('from', 'name')),
    to_id = unlistWithNA(json, c('to', 'data', "1", 'id')),
    to_name = unlistWithNA(json, c('to', 'data', '1', 'name')),
    message = unlistWithNA(json, 'message'),
    created_time = unlistWithNA(json, 'created_time'),
    type = unlistWithNA(json, 'type'),
    link = unlistWithNA(json, 'link'),
    id = unlistWithNA(json, 'id'),
    likes_count = unlistWithNA(json, c('likes', 'summary', 'total_count')),
    comments_count = unlistWithNA(json, c('comments', 'summary', 'total_count')),
    shares_count = unlistWithNA(json, c('shares', 'count')),
    stringsAsFactors=F)
  return(df)
}

pageDataToDF <- function(json){
  df <- data.frame(
    from_id = unlistWithNA(json, c('from', 'id')),
    from_name = unlistWithNA(json, c('from', 'name')),
    message = unlistWithNA(json, 'message'),
    created_time = unlistWithNA(json, 'created_time'),
    type = unlistWithNA(json, 'type'),
    link = unlistWithNA(json, 'link'),
    id = unlistWithNA(json, 'id'),
    story = unlistWithNA(json, 'story'),
    likes_count = unlistWithNA(json, c('likes', 'summary', 'total_count')),
    comments_count = unlistWithNA(json, c('comments', 'summary', 'total_count')),
    shares_count = unlistWithNA(json, c('shares', 'count')),
    stringsAsFactors=F)
  return(df)
}

insightsDataToDF <- function(x){
  
  values <- list()
  
  if(grepl('^post',x$data[[1]]$name)){
    
    for (i in 1:length(x$data[[1]]$values)){
      tmp <- data.frame(unlist(x$data[[1]]$values[[i]]$value), stringsAsFactors=F)
      tmp$variable <- row.names(tmp)
      row.names(tmp) <- NULL
      names(tmp) <- c('value', 'variable')
      values[[i]] <- tmp
    }  
  } else { 
    
    for (i in 1:length(x$data[[1]]$values)){
      tmp <- data.frame(unlist(x$data[[1]]$values[[i]]$value), end_time=x$data[[1]]$values[[i]]$end_time, stringsAsFactors=F)
      tmp$variable <- row.names(tmp)
      row.names(tmp) <- NULL
      names(tmp) <- c('value', 'end_time', 'variable')
      values[[i]] <- tmp
    }
  }
  
  values <- do.call('rbind',values)
  
  df <- data.frame(
    id=x$data[[1]]$id,
    name=x$data[[1]]$name,
    period=x$data[[1]]$period,
    title=x$data[[1]]$title,
    description=x$data[[1]]$description,
    values,
    stringsAsFactors=FALSE
  )
  
  if(length(unique(df$variable))==1 & df$variable[1]==1){
    df$variable <- NULL
  } else {
    df <- df
  }
  
  return(df)
}

myPostDataToDF <- function(json){
  df <- data.frame(
    from_id = ifelse(!is.null(json$from$id),json$from$id, NA),
    # from_name = ifelse(!is.null(json$from$name), json$from$name, NA),   ### from_name is no longer allowed by FB ###
    message = ifelse(!is.null(json$message),json$message, NA),
    created_time = json$created_time,
    # type = json$type,
    # link = ifelse(!is.null(json$link), json$link, NA),                  ### type and link are also deprecated
    id = json$id,
    likes_count = ifelse(!is.null(json$likes$summary$total_count),
                         json$likes$summary$total_count, 0),
    comments_count = ifelse(!is.null(json$comments$summary$total_count),
                            json$comments$summary$total_count, 0),
    shares_count = ifelse(!is.null(json$shares$count),
                          json$shares$count, 0),
    stringsAsFactors=F)
  return(df)
}

reactionsDataToDF <- function(json){
  if (!is.null(json)){
    df <- data.frame(
      from_name = unlistWithNA(json, "name"),
      from_type = unlistWithNA(json, "type"),
      from_id = unlistWithNA(json, "id"),
      stringsAsFactors=F
    )
  }
  if (length(json)==0){
    df <- NULL
  }
  return(df)
}


likesDataToDF <- function(json){
  if (!is.null(json)){
    df <- data.frame(
      from_name = unlistWithNA(json, "name"),
      from_id = unlistWithNA(json, "id"),
      stringsAsFactors=F
    )
  }
  if (length(json)==0){
    df <- NULL
  }
  return(df)
}

commentsDataToDF <- function(json){
  if (!is.null(json)){
    df <- data.frame(
      from_id = unlistWithNA(json, c('from', 'id')),
      from_name = unlistWithNA(json, c('from', 'name')),
      message = unlistWithNA(json, 'message'),
      created_time = unlistWithNA(json, 'created_time'),
      likes_count = unlistWithNA(json, 'like_count'),
      comments_count = unlistWithNA(json, 'comment_count'),
      id = unlistWithNA(json, 'id'),
      stringsAsFactors=F)
  }
  if (is.null(json)){
    df <- NULL
  }
  return(df)
}

repliesDataToDF <- function(json){
  if (!is.null(json)){
    df <- data.frame(
      from_id = unlistWithNA(json, c('from', 'id')),
      from_name = unlistWithNA(json, c('from', 'name')),
      message = unlistWithNA(json, 'message'),
      created_time = unlistWithNA(json, 'created_time'),
      likes_count = unlistWithNA(json, 'like_count'),
      id = unlistWithNA(json, 'id'),
      stringsAsFactors=F)
  }
  if (is.null(json)){
    df <- NULL
  }
  return(df)
}

userDataToDF <- function(user_data, private_info){
  df <- data.frame(
    id = unlistWithNA(user_data, 'id'),
    name = unlistWithNA(user_data, 'name'),
    username = unlistWithNA(user_data, 'username'),
    first_name = unlistWithNA(user_data, 'first_name'),
    middle_name = unlistWithNA(user_data, 'middle_name'),
    last_name = unlistWithNA(user_data, 'last_name'),
    gender = unlistWithNA(user_data, 'gender'),
    locale = unlistWithNA(user_data, 'locale'),
    #category = unlistWithNA(user_data, 'category'),
    likes = unlistWithNA(user_data, 'fan_count'),
    picture = unlistWithNA(user_data, c('picture', 'data', 'url')),
    stringsAsFactors=F)
  if (private_info==TRUE){
    df$birthday <- unlistWithNA(user_data, 'birthday')
    df$location <- unlistWithNA(user_data, c('location', 'name'))
    df$hometown <- unlistWithNA(user_data, c('hometown', 'name'))
    df$relationship_status <- unlistWithNA(user_data, 'relationship_status')
  }
  return(df)
}

checkinDataToDF <- function(checkin_data){
  df <- data.frame(
    checkin_time = unlistWithNA(checkin_data, 'created_time'),
    place_id = unlistWithNA(checkin_data, c('place', 'id')),
    place_name = unlistWithNA(checkin_data, c('place', 'name')),
    place_city = unlistWithNA(checkin_data, c('place', 'location','city')),
    place_state = unlistWithNA(checkin_data, c('place', 'location','state')),
    place_country = unlistWithNA(checkin_data, c('place', 'location','country')),
    place_lat = unlistWithNA(checkin_data, c('place', 'location', 'latitude')),
    place_long = unlistWithNA(checkin_data, c('place', 'location', 'longitude')),
    stringsAsFactors=F)
  return(df)
}

userLikesToDF <- function(user_likes){
  df <- data.frame(
    id = unlistWithNA(user_likes, 'id'),
    names = unlistWithNA(user_likes, 'name'),
    website = unlistWithNA(user_likes, 'website'),
    stringsAsFactors=F)
  return(df)
}

sharesToDF <- function(shares){
  df <- data.frame(
    from_id = unlistWithNA(shares, c('from', 'id')),
    from_name = unlistWithNA(shares, c('from', 'name')),
    shared_time = unlistWithNA(shares, 'created_time'),
    id = unlistWithNA(shares, 'id'),
    stringsAsFactors=F)
  return(df)
}


tagsDataToDF <- function(tags){
  tags <- lapply(tags, '[[', "tags")
  tags <- lapply(tags, '[[', 'data')
  tagsListToDF <- function(x){
    if (!is.null(x)){
      values <- data.frame(matrix(unlist(x),ncol=2,byrow=TRUE),
                           stringsAsFactors=F)
      names(values) <- c("id", "name")	
    }
    if (is.null(x)){
      values <- NULL
    }
    return(values)
  }
  tags <- lapply(tags, tagsListToDF)
  return(tags)
}

replyDataToDF <- function(json){
  df <- data.frame(
    from_id = ifelse(!is.null(json$from$id), json$from$id, NA),
    from_name = ifelse(!is.null(json$from$name), json$from$name, NA),
    message = ifelse(!is.null(json$message),json$message, NA),
    created_time = json$created_time,
    likes_count = json$like_count,
    comments_count = json$comment_count,
    id = json$id,
    stringsAsFactors=F)
  return(df)
}

#myUnlistWithNA <- function(lst, field){
unlistWithNA <- function(lst, field){
  if (length(field)==1){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field]]))
  }
  if (length(field)==2){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
  }
  if (field[1]=="shares"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(0, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
  }
  if (length(field)==3){
    notnulls <- unlist(lapply(lst, function(x) 
      tryCatch(!is.null(x[[field[1]]][[field[2]]][[field[3]]]), 
               error=function(e) FALSE)))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  if (length(field)==4 & field[1]=="to"){
    notnulls <- unlist(lapply(lst, function(x) 
      tryCatch(!is.null(x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]), 
               error=function(e) FALSE)))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
  }
  if (field[1] %in% c("comments", "likes") & !is.na(field[2])){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
    vect <- rep(0, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  return(vect)
}

searchPageDataToDF <- function(json){
  df <- data.frame(
    id = unlistWithNA(json, 'id'),
    link = unlistWithNA(json, 'link'),
    city = unlistWithNA(json, c('location', 'city')),
    state = unlistWithNA(json, c('location', 'state')),
    country = unlistWithNA(json, c('location', 'country')),
    latitude = unlistWithNA(json, c('location', 'latitude')),
    longitude = unlistWithNA(json, c('location', 'longitude')),
    name = unlistWithNA(json, 'name'),
    verified = unlistWithNA(json, 'verification_status'),
    unclaimed = unlistWithNA(json, 'is_unclaimed'),
    stringsAsFactors=F)
  return(df)
}

eventDataToDF <- function(json){
  df <- data.frame(
    id = unlistWithNA(json, 'id'),
    name = unlistWithNA(json, 'name'),
    description = unlistWithNA(json, 'description'),
    start_time = unlistWithNA(json, 'start_time'),
    end_time = unlistWithNA(json, 'end_time'),
    place_name = unlistWithNA(json, c("place", "name")),
    attending_count = unlistWithNA(json, 'attending_count'),
    declined_count = unlistWithNA(json, 'declined_count'),
    maybe_count = unlistWithNA(json, 'maybe_count'),
    noreply_count = unlistWithNA(json, 'noreply_count'),
    stringsAsFactors=F)
  return(df)
}



callAPI <- function(url, token, api=NULL){
  # disable http/2, it is causing problems!!!
  httr::set_config(httr::config(http_version = 0))
  # h <- curl::new_handle()
  # curl::handle_setopt(h, http_version = 2)
  
  if (!is.null(api) & !grepl("v2\\..", url)){
    # if api version not in URL already
    url <- gsub("facebook.com/", paste0("facebook.com/", api, "/"), url)
  }
  if (class(token)[1]=="config"){
    url.data <- GET(url, config=token)
  }
  if (class(token)[1]=="Token2.0"){
    url.data <- GET(url, config(token=token))
  }	
  if (class(token)[1]=="character"){
    url <- paste0(url, "&access_token=", token)
    url <- gsub(" ", "%20", url)
    url.data <- GET(url)
  }
  if (class(token)[1]!="character" & class(token)[1]!="config" & class(token)[1]!="Token2.0"){
    stop("Error in access token. See help for details.")
  }
  content <- rjson::fromJSON(rawToChar(url.data$content))
  if (length(content$error)>0){
    stop(content$error$message)
  }	
  return(content)
}

getTokenVersion <- function(token){
  
  if (!is.na(class(token)[4])){
    tkversion <- class(token)[4]
  }
  if (is.na(class(token)[4])){
    error <- tryCatch(callAPI('https://graph.facebook.com/pablobarbera', token),
                      error = function(e) e)
    if (inherits(error, 'error')){
      tkversion <- 'v2'
    }
    if (!inherits(error, 'error')){
      tkversion <- 'v1'
    }
  }
  return(tkversion)
  
}


formatFbDate <- function(datestring, format="datetime") {
  if (format=="datetime"){
    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")    
  }
  if (format=="date"){
    date <- as.Date(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")   
  }
  return(date)
}