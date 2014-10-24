library(shiny)
library(data.table)
library(tm)
library(twitteR)
library(wordcloud)
library(RCurl)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(ggmap)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
load("credential/twitter.RData")
registerTwitterOAuth(twitCred)

shinyServer(function(input, output) {
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
        n=800,
        geocode=paste0(c(paste0(unique(profile_data[twitter_hashtag==term, list(lat,lng)]), collapse=","), "10mi"), collapse=",")
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
  
  rawTweets <- reactive({getTweets(input$twitter_hashtag, input$twitter_period[1], input$twitter_period[2])})
  tweetDate <- reactive({dailyTweets(rawTweets())})
  tweetWords <- reactive({parseTweets(input$twitter_hashtag, rawTweets())})
  
  overviewData <- reactive({
    filterCategory <- input$filter_category
    filterStyle <- input$filter_style
    filterCountry <- input$filter_country
    filterYear <- input$filter_year
    filterLanguage <- input$filter_language
    output <- profile_data[
      (category==filterCategory | filterCategory=="All") &
        (style==filterStyle | filterStyle=="All") &
        (country==filterCountry | filterCountry=="All") &
        (event_year==filterYear | filterYear=="All") &
        (language==filterLanguage | filterLanguage=="All")
      ]
    output
  })
  
  output$overview_plot <- renderPlot({
    input_x <- input$xaxis
    input_y <- input$yaxis
    data <- overviewData()[,
                           list(
                             talks=length(video_id),
                             views=sum(views),
                             shares=sum(shares),
                             estimatedMinutesWatched=sum(estimatedMinutesWatched),
                             averageViewPercentage=mean(averageViewPercentage),
                             subscribersGained=sum(subscribersGained)
                           ),
                           keyby=input_x]
    ggplot(data, aes_string(x=input_x, y=input_y)) +
      geom_bar(stat="identity", color="black", fill="#2ca25f", width=0.5) +
      geom_text(data=data, aes_string(label=input_y, vjust=-0.8)) +
      scale_y_continuous(labels=comma) +
      theme(
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15, vjust=1)
      )
  }, width=1024)
  
  output$category_plot <- renderPlot({
    input_x <- input$xaxis
    input_y <- input$yaxis
    data <- overviewData()[,
                           list(
                             talks=length(video_id),
                             views=sum(views),
                             shares=sum(shares),
                             estimatedMinutesWatched=sum(estimatedMinutesWatched),
                             averageViewPercentage=mean(averageViewPercentage),
                             subscribersGained=sum(subscribersGained)
                           ),
                           keyby=c(input_x, "category")]
    ggplot(data, aes_string(x=input_x, y=input_y)) +
      geom_bar(stat="identity", color="black", fill="#2ca25f", width=0.5) +
      geom_text(data=data, aes_string(label=input_y, vjust=-0.3), size=3) +
      scale_y_continuous(labels=comma) +
      facet_wrap(~category, scales="free_y") +
      theme(
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15, vjust=1)
      )
  }, width=1024)
  
  output$style_plot <- renderPlot({
    input_x <- input$xaxis
    input_y <- input$yaxis
    data <- overviewData()[,
                           list(
                             talks=length(video_id),
                             views=sum(views),
                             shares=sum(shares),
                             estimatedMinutesWatched=sum(estimatedMinutesWatched),
                             averageViewPercentage=mean(averageViewPercentage),
                             subscribersGained=sum(subscribersGained)
                           ),
                           keyby=c(input_x, "style")]
    ggplot(data, aes_string(x=input_x, y=input_y)) +
      geom_bar(stat="identity", color="black", fill="#2ca25f", width=0.5) +
      geom_text(data=data, aes_string(label=input_y, vjust=-0.3), size=3) +
      scale_y_continuous(labels=comma) +
      facet_wrap(~style, scales="free_y") +
      theme(
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15, vjust=1)
      )
  }, width=1024)
  
  output$map_plot <- renderPlot({
    input_mapCircle <- input$map_circle
    input_country <- input$map_country
    input_zoom <- input$zoom_scale
    input_size <- input$map_bubble_size
    if (input_country == "World") {
      data <- profile_data[,
                           list(
                             talks=length(video_id),
                             views=sum(views),
                             shares=sum(shares),
                             estimatedMinutesWatched=sum(estimatedMinutesWatched),
                             averageViewPercentage=mean(averageViewPercentage),
                             subscribersGained=sum(subscribersGained)
                           ),
                           keyby=c("lat", "lng")]
      ggplot() +
        borders("world", colour="gray65", fill="#f9f9f9") +
        geom_point(aes_string(x="lng", y="lat", size=input_mapCircle), alpha=0.3, color="#2ca25f", data=data) +
        scale_size_continuous(range=c(input_size[1], input_size[2])) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data)
    } else {
      data <- profile_data[country==input_country,
                           list(
                             talks=length(video_id),
                             views=sum(views),
                             shares=sum(shares),
                             estimatedMinutesWatched=sum(estimatedMinutesWatched),
                             averageViewPercentage=mean(averageViewPercentage),
                             subscribersGained=sum(subscribersGained)
                           ),
                           keyby=c("lat", "lng")]
      map <- get_googlemap(input_country, zoom=input_zoom, marker=geocode(input_country), maptype="terrain", color="bw")
      ggmap(map) +
        geom_point(aes_string(x="lng", y="lat", size=input_mapCircle), alpha=0.5, color="#2ca25f", data=data) +
        scale_size_continuous(range=c(input_size[1], input_size[2])) +
        geom_point(aes(x=lng, y=lat), size=2, color="#2ca25f", data=data)
    }
  }, width=1024)
  
  output$map_cat_plot <- renderPlot({
    input_mapCircle <- input$map_circle
    input_country <- input$map_country
    input_zoom <- input$zoom_scale
    input_size <- input$map_bubble_size
    if (input_country == "World") {
      data <- profile_data[,
                           list(
                             talks=length(video_id),
                             views=sum(views),
                             shares=sum(shares),
                             estimatedMinutesWatched=sum(estimatedMinutesWatched),
                             averageViewPercentage=mean(averageViewPercentage),
                             subscribersGained=sum(subscribersGained)
                           ),
                           keyby=c("lat", "lng", "category")]
      ggplot() +
        borders("world", colour="gray65", fill="#f9f9f9") +
        geom_point(aes_string(x="lng", y="lat", size=input_mapCircle), alpha=0.3, color="#2ca25f", data=data) +
        scale_size_continuous(range=c(input_size[1], input_size[2])) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data) +
        facet_wrap(~category) +
        theme(strip.background=element_rect(fill="#2ca25f"))
    } else {
      data <- profile_data[country==input_country,
                           list(
                             talks=length(video_id),
                             views=sum(views),
                             shares=sum(shares),
                             estimatedMinutesWatched=sum(estimatedMinutesWatched),
                             averageViewPercentage=mean(averageViewPercentage),
                             subscribersGained=sum(subscribersGained)
                           ),
                           keyby=c("lat", "lng", "category")]
      map <- get_googlemap(input_country, zoom=input_zoom, marker=geocode(input_country), maptype="terrain", color="bw")
      ggmap(map) +
        geom_point(aes_string(x="lng", y="lat", size=input_mapCircle), alpha=0.5, color="#2ca25f", data=data) +
        scale_size_continuous(range=c(input_size[1], input_size[2])) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data) +
        facet_wrap(~category) +
        theme(strip.background=element_rect(fill="#2ca25f"))
    }
  }, width=1024)
  
  output$map_style_plot <- renderPlot({
    input_mapCircle <- input$map_circle
    input_country <- input$map_country
    input_zoom <- input$zoom_scale
    input_size <- input$map_bubble_size
    if (input_country == "World") {
      data <- profile_data[,
                           list(
                             talks=length(video_id),
                             views=sum(views),
                             shares=sum(shares),
                             estimatedMinutesWatched=sum(estimatedMinutesWatched),
                             averageViewPercentage=mean(averageViewPercentage),
                             subscribersGained=sum(subscribersGained)
                           ),
                           keyby=c("lat", "lng", "style")]
      ggplot() +
        borders("world", colour="gray65", fill="#f9f9f9") +
        geom_point(aes_string(x="lng", y="lat", size=input_mapCircle), alpha=0.3, color="#2ca25f", data=data) +
        scale_size_continuous(range=c(input_size[1], input_size[2])) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data) +
        facet_wrap(~style) +
        theme(strip.background=element_rect(fill="#2ca25f"))
    } else {
      data <- profile_data[country==input_country,
                           list(
                             talks=length(video_id),
                             views=sum(views),
                             shares=sum(shares),
                             estimatedMinutesWatched=sum(estimatedMinutesWatched),
                             averageViewPercentage=mean(averageViewPercentage),
                             subscribersGained=sum(subscribersGained)
                           ),
                           keyby=c("lat", "lng", "style")]
      map <- get_googlemap(input_country, zoom=input_zoom, marker=geocode(input_country), maptype="terrain", color="bw")
      ggmap(map) +
        geom_point(aes_string(x="lng", y="lat", size=input_mapCircle), alpha=0.5, color="#2ca25f", data=data) +
        scale_size_continuous(range=c(input_size[1], input_size[2])) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data) +
        facet_wrap(~style) +
        theme(strip.background=element_rect(fill="#2ca25f"))
    }
  }, width=1024)
  
  output$search_output <- renderDataTable({
    profile_data[, list(youtube_url, title, speaker, language, upload_time,
                        category, style, taxonomy, human_tags, notes,
                        overall_rating, idea_rating, presentation_rating, video_quality,
                        event, city, country, starts_at, ends_at, twitter_hashtag)]
  })
  
  output$word_cloud <- renderPlot({
    input$search_tweets
    wordcloud(tweetWords()$word, tweetWords()$freq, scale=c(8, 0.3), min.freq=3, random.order=FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2"))
  }, width=1024)
  
  output$tweets <- renderDataTable({
    input$search_tweets
    tweetDate()
  })
  
  output$twitter_ts <- renderPlot({
    input_measure <- input$twitter_ts_y
    raw_data <- tweetDate()[, list(tweets=length(tweet_text)), keyby=date]
    date_data <- data.table("date"=as.Date(min(raw_data$date):max(raw_data$date), origin="1970-01-01"), key="date")
    data <- merge(date_data, raw_data, all.x=TRUE)
    data[is.na(tweets), tweets:=0]
    ggplot(data, aes_string(x="date", y=input_measure)) +
      geom_point() +
      geom_line() +
      scale_x_date(labels=date_format("%m-%d")) +
      scale_y_continuous(labels=comma) +
      theme(
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15, vjust=1)
      )
  }, width=1024)
  
  output$loc_trend <- renderDataTable({
    input_event <- input$trend_location
    lat_long <- unique(profile_data[event==input_event, list(lat, lng)])
    loc_id <- closestTrendLocations(lat_long$lat, lat_long$lng)$woeid
    unique(rbind(getTrends(loc_id), getTrends(loc_id, exclude="hashtags")))
  })
  
})

