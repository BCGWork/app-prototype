#################
#### load library
#################
library(data.table)
library(reshape2)
library(igraph)
library(whisker)


##############
#### functions
##############
#### video tags processing
utilReset <- function(x) {
  if (length(x) >= 1) {
    return(1)
  } else {
    return(0)
  }
}

processTag <- function(data) {
  tmp <- melt(data, id.vars="video_id")
  tmp <- tmp[value!=""]
  utag <- unique(tmp$value)[order(unique(tmp$value))]
  tag_list <- data.table(ID=0:(length(utag)-1), tag=utag)
  setkey(tag_list, tag)
  setkey(tmp, value)
  tmp2 <- tmp[tag_list]
  mat <- dcast.data.table(tmp2, video_id~ID, fun=utilReset)
  mat[, video_id:=NULL]
  adj_mat <- crossprod(as.matrix(mat))
  tag_list$count <- diag(adj_mat)
  igraph_mat <- simplify(graph.adjacency(adj_mat, mode="undirected", weighted=TRUE))
  edge_list <- get.data.frame(igraph_mat)
  edge_list$from <- as.numeric(edge_list$from)
  edge_list$to <- as.numeric(edge_list$to)
  return(list(adj_mat=adj_mat, edge_list=edge_list, node_list=tag_list))
}

processTagComb <- function(data, comb) {
  p <- ncol(data)
  dt <- copy(data)
  for (i in names(dt)) {set(dt, which(dt[[i]]==""), i, NA)}
  dt[, tag_comb:=p-1-rowSums(is.na(dt))]
  dt[, tag:=gsub("NA, ", "", gsub(", NA", "", apply(dt[,2:p,with=FALSE], 1, function(x){paste(x, collapse=", ")})))]
  dt <- dt[tag!="NA"]
  out <- dt[, list(count=length(unique(video_id))), by=list(tag_comb, tag)]
  out[, ID:=1:nrow(out)]
  return(list(tag_freq=out[tag_comb==comb, list(ID, tag, count)], raw=dt))
}

## function to identify evolution of combinations of tags
tagCombinationsEvol <- function(data1, data2, comb) {
  finalComb <- processTagComb(data1, comb)$tag_freq
  compComb <- processTagComb(data2, comb)$tag_freq
  finalTags <- finalComb[, count, key=tag]
  compTags <- compComb[, count, key=tag]
  tagDT <- merge(compTags, finalTags, all=TRUE)
  setnames(tagDT, c("count.x", "count.y"), c("before", "after"))
  for (i in names(tagDT)) {set(tagDT, which(is.na(tagDT[[i]])), i, 0)}
  tagDT[, delta:=after-before]
  tagDT[, percent_change:=paste0(100*(after-before)/before, "%")]
  tagDT[before==0, status:="new tag"]
  tagDT[after==0, status:="disappeared tag"]
  tagDT[percent_change=="Inf%", percent_change:="not applicable"]
  tagDT[is.na(status), status:="existing tag"]
  return(tagDT[, list(tag, delta, percent_change, status)][order(-rank(delta))])
}


#### d3Network Adapted
source("d3Adapted.R")

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
tag_data <- fread("data/tag_data_v1.csv", header=TRUE, sep=";")
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

## TED talks tags
ted_data <- fread("data/TEDtalks_tag_data.csv", header=TRUE, sep=",")
ted_data <- ted_data[, Published:=as.Date(Published, format="%m/%d/%y")]
ted_data <- ted_data[!is.na(Published), event_year:=year(Published)]
