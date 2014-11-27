#################
#### load library
#################
library(data.table)
library(reshape2)
library(igraph)
library(d3Network)



##############
#### functions
##############
#### video tags processing
processTag <- function(data) {
  tmp <- melt(data, id.vars="video_id")
  tmp <- tmp[value!=""]
  utag <- unique(tmp$value)[order(unique(tmp$value))]
  tag_list <- data.table(ID=0:(length(utag)-1), tag=utag)
  setkey(tag_list, tag)
  setkey(tmp, value)
  tmp2 <- tmp[tag_list]
  mat <- dcast.data.table(tmp2, video_id~ID, fun=length)
  mat[, video_id:=NULL]
  adj_mat <- crossprod(as.matrix(mat))
  igraph_mat <- simplify(graph.adjacency(adj_mat, mode="undirected", weighted=TRUE))
  edge_list <- get.data.frame(igraph_mat)
  edge_list$from <- as.numeric(edge_list$from)
  edge_list$to <- as.numeric(edge_list$to)
  return(list(adj_mat=adj_mat, edge_list=edge_list, node_list=tag_list))
}

#### social media analysis
getTweets <- function(term, start_date, end_date) {
  if (term=="#TEDxTalks") {
    data <- searchTwitter(
      term,
      since=as.character(start_date),
      until=as.character(end_date),
      lang="en",
      n=800
    )
  } else {
    data <- searchTwitter(
      term,
      since=as.character(start_date),
      until=as.character(end_date),
      lang="en",
      n=800
      #         geocode=paste0(c(paste0(unique(profile_data[twitter_hashtag==term, list(lat,lng)]), collapse=","), "10mi"), collapse=",")
    )
  }
  return(data)
}

dailyTweets <- function(data) {
  if (length(data) > 0) {
    orig_tweets <- data[!(unlist(lapply(data, function(x){x$getRetweeted()})))]
  } else {
    stop("No tweets found within specified date range.")
  }
  tweet_text <- sapply(orig_tweets, function(x) {x$getText()})
  tweet_date <- sapply(orig_tweets, function(x) {as.Date(x$getCreated())})
  return(data.table("date"=as.Date(tweet_date, origin="1970-01-01"), "tweet_text"=tweet_text))
}

parseTweets <- function(term, data) {
  if (length(data) > 0) {
    orig_tweets <- data[!(unlist(lapply(data, function(x){x$getRetweeted()})))]
  } else {
    stop("No tweets found within specified date range.")
  }
  data_text <- sapply(orig_tweets, function(x) x$getText())
  data_corpus <- Corpus(VectorSource(data_text))
  tdm <- TermDocumentMatrix(
    data_corpus,
    control=list(
      removePunctuation=TRUE,
      removeNumbers=TRUE,
      tolower=TRUE,
      stopwords=c(tolower(term), tolower(gsub("#", "", term)), "tedx", stopwords("english"))
    )
  )
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE) 
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
}



##############
#### load data
##############
## cleaned video data
profile_data <- fread("data/clean_data_v4.csv", header=TRUE, sep=",")
tag_data <- fread("data/tag_data_v1.csv", header=TRUE, sep=",")
profile_data <- profile_data[!is.na(video_id)]
profile_data[, starts_at:=as.Date(starts_at)]
profile_data[, ends_at:=as.Date(ends_at)]
profile_data[!is.na(starts_at), event_year:=year(starts_at)]
profile_data$overall_rating <- factor(profile_data$overall_rating, levels=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F"))
profile_data$idea_rating <- factor(profile_data$idea_rating, levels=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F"))
profile_data$presentation_rating <- factor(profile_data$presentation_rating, levels=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F"))
profile_data$video_quality <- factor(profile_data$video_quality, levels=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F"))
profile_data[twitter_hashtag=="", twitter_hashtag:=paste0("#", event)]
setkey(profile_data, video_id)
setkey(tag_data, video_id)
profile_data <- merge(profile_data, tag_data, all.x=TRUE)

## tag library
content_tags <- processTag(profile_data[, list(video_id, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5)])$node_list$tag
format_tags <- processTag(profile_data[, list(video_id, Format_tag1, Format_tag2, Format_tag3)])$node_list$tag
intent_tags <- processTag(profile_data[, list(video_id, Intent_tag1, Intent_tag2)])$node_list$tag

