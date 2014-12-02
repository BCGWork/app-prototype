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
  if (comb <= 2) {
    result <- processTag(data)
    if (comb == 1) {
      return(result$node_list);
    } else {
      # We use the edge list to find the combinations
      tag_list <- result$node_list
      edge_list <- result$edge_list
      IDs <- paste(tag_list[(edge_list$from + 1),]$ID, tag_list[(edge_list$to + 1),]$ID, sep=", ")
      tag <- paste(tag_list[(edge_list$from + 1),]$tag, tag_list[(edge_list$to + 1),]$tag, sep=", ")
      count <- edge_list$weight
      return(data.frame(ID=IDs, tag=tag, count=count));
    }
  } else {
    # comb = 3 or 4
    # no other choice than go through a FOR loop
    tmp <- melt(data, id.vars="video_id")
    tmp <- tmp[value!=""]
    utag <- unique(tmp$value)[order(unique(tmp$value))]
    tag_list <- data.table(ID=0:(length(utag)-1), tag=utag)
    setkey(tag_list, tag)
    setkey(tmp, value)
    tmp2 <- tmp[tag_list]
    # Remove talks without not enough tags      
    mat <- dcast.data.table(tmp2, video_id~ID, fun=utilReset)
    mat[, video_id:=NULL]
    mat <- mat[which(rowSums(mat) >= comb), ]
    IDs <- c()
    tag <- c()
    count <- c()
    for (talk in 1:nrow(mat)) {
      tags <- which(mat[talk,] == 1)
      # Generate list of combinations
      combinations <- combn(tags, comb)
      # For each combinations, verify if it exists to create it or add one
      for (j in 1:ncol(combinations)) {
        combinations[,j] = combinations[order(combinations[,j]),j]
        occ = which(IDs==paste(combinations[,j], collapse=", "))
        if (length(occ) == 0) {
          # If not, we create the combination
          IDs <- c(IDs, paste(combinations[,j], collapse=", "))
          tag <- c(tag, paste(tag_list[combinations[,j], ]$tag, collapse = ", "))
          count <- c(count, 1)
        } else {
          # If yes, we increase the number of occurrences
          count[occ] = count[occ] + 1
        }
      }
    }
    return (data.frame(ID = IDs, tag = tag, count = count))   
  }
}

## function to identify evolution of combinations of tags
tagCombinationsEvol <- function(data1, data2, comb, output="all") {
  
  finalComb <- processTagComb(data1, comb)
  compComb <- processTagComb(data2, comb)
  
  label <- c()
  delta <- c()
  percent <- c()
  comment <- c()
  
  # Identify lines that are in both dataframes
  in_both <- unique(as.vector(compComb[as.vector(compComb$ID) %in% as.vector(finalComb$ID),]$ID))
  
  if (output == "all") {
    
    if (length(in_both) >= 1) {
      for (i in 1:length(in_both)) {
        ln_final = grep(paste("^",in_both[i],"$",sep=""), finalComb$ID)
        ln_comp = grep(paste("^",in_both[i],"$",sep=""), compComb$ID)
        # cat(paste("Looking for: ",in_both[i]," ; Final: ",ln_final, " ; Comp: ",ln_comp,"\n",sep=""))
        if (length(ln_final) > 1) cat (paste("!!! for ", in_both[i], "\n", sep=""))
        if (length(ln_comp) > 1) cat (paste("!!! for ", in_both[i], "\n", sep=""))
        label <- c(label, as.character(finalComb[ln_final,]$tag))
        delta <- c(delta, finalComb[ln_final,]$count - compComb[ln_comp,]$count)
        percent <- c(percent, as.character(paste(as.character(round(delta[i] / compComb[ln_comp,]$count*100)),"%",sep="")))
        comment <- c(comment, "-")
      }
    }
    
  }
  
  if (output == "all" | output == "new") {
    
    # Add lines that are in the end and not in the start
    label <- c(label, as.character(finalComb[!(finalComb$ID %in% in_both),]$tag))
    delta <- c(delta, finalComb[!(finalComb$ID %in% in_both),]$count)
    percent <- c(percent, rep("+infty%",length(finalComb[!(finalComb$ID %in% in_both),]$tag)))
    comment <- c(comment, rep("New",length(finalComb[!(finalComb$ID %in% in_both),]$tag)))
    
  }
  
  if (output == "all" | output == "dis") {
    
    # Add lines that are in the start and not in the end
    label <- c(label, as.character(compComb[!(compComb$ID %in% in_both),]$tag))
    delta <- c(delta, - compComb[!(compComb$ID %in% in_both),]$count)
    percent <- c(percent, rep("-100%",length(compComb[!(compComb$ID %in% in_both),]$tag)))
    comment <- c(comment, rep("Disappeared",length(compComb[!(compComb$ID %in% in_both),]$tag)))
    
  }
  
  result <- data.frame(tag = label, delta = delta, percent = percent, comment = comment)
  result <- result[order(result$delta, decreasing=TRUE),]
  return(result);
  
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

##############
## TED talks tags
ted_data <- fread("data/TEDtalks_tag_data.csv", header=TRUE, sep=",")
ted_data <- ted_data[, Published:=as.Date(Published, format="%m/%d/%y")]
ted_data <- ted_data[!is.na(Published), event_year:=year(Published)]
