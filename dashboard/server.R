library(shiny)
library(data.table)
library(reshape2)
library(tm)
library(twitteR)
library(wordcloud)
library(RCurl)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(ggmap)
library(igraph)
library(d3Network)


options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
load("credential/twitter.RData")
registerTwitterOAuth(twitCred)

shinyServer(function(input, output) {
  ###########################
  #### reactive data listener
  tagNetworkData <- reactive({
    tagType <- input$tag_type
    tagCountry <- input$tag_country
    tagPeriod <- input$tag_period
    
    if (tagType=="Topic") {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5)
        ]
      output <- processTag(f_out)
    } else if (tagType=="Delivery Format") {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Format_tag1, Format_tag2, Format_tag3)
        ]
      output <- processTag(f_out)
    } else {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Intent_tag1, Intent_tag2)
        ]
      output <- processTag(f_out)
    }
    output
  })
  
  rawTweets <- reactive({getTweets(input$twitter_hashtag, input$twitter_period[1], input$twitter_period[2])})
  tweetDate <- reactive({dailyTweets(rawTweets())})
  tweetWords <- reactive({parseTweets(input$twitter_hashtag, rawTweets())})
  
  overviewData <- reactive({
    filterPeriod <- input$filter_period
    filterTag <- input$filter_tag
    filterCountry <- input$filter_country
    filterLanguage <- input$filter_language
    output <- profile_data[
      (starts_at>=filterPeriod[1] & ends_at<=filterPeriod[2]) &
        (country==filterCountry | filterCountry=="All") &
        (language==filterLanguage | filterLanguage=="All") &
        (is.null(filterTag) | Content_tag1 %in% filterTag | Content_tag2 %in% filterTag | Content_tag3 %in% filterTag | Content_tag4 %in% filterTag | Content_tag5 %in% filterTag)
      ]
    output
  })
  
  mapData <- reactive({
    mapPeriod <- input$map_period
    mapTag <- input$map_tag
    output <- profile_data[
      (starts_at>=mapPeriod[1] & ends_at<=mapPeriod[2]) &
        (is.null(mapTag) | Content_tag1 %in% mapTag | Content_tag2 %in% mapTag | Content_tag3 %in% mapTag | Content_tag4 %in% mapTag | Content_tag5 %in% mapTag)
      ]
    output
  })
  
  
  ##################
  #### server output
  output$tagNetwork <- renderPrint({
    data <- tagNetworkData()
    adj_mat <- data$adj_mat
    links <- data$edge_list
    nodes <- data$node_list
    tagCluster <- input$tag_cluster
    cl <- kmeans(adj_mat, tagCluster, iter.max=100, nstart=30)$cluster
    cl_dt <- data.table(ID=as.numeric(names(cl)), group=cl)
    setkey(nodes, ID)
    setkey(cl_dt, ID)
    nodes <- merge(nodes, cl_dt)
    
    d3ForceNetwork(Links=links, Node=as.data.frame(nodes), Source="from", Target="to",
                   Value="weight", NodeID="tag", Group="group", width=900, height=600,
                   opacity=0.8, standAlone=FALSE, parentElement="#tagNetwork")
  }, width=1024)
  
  output$mostUsedTags <- renderDataTable({
    data <- tagNetworkData()
    adj_mat <- data$adj_mat
    nodes <- data$node_list
    tagFrequency <- rowSums(adj_mat)
    frequentTags <- data.table(ID=as.numeric(names(tagFrequency)), frequency=tagFrequency)
    setkey(nodes, ID)
    setkey(frequentTags, ID)
    nodes[frequentTags][order(-rank(frequency))]
  })
  
  #   output$tagEvol <- renderDataTable({})
  #   output$tagNew <- renderDataTable({})
  #   output$tagDis <- renderDataTable({})
  
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
      data <- mapData()[,
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
      data <- mapData()[country==input_country,
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
      data <- mapData()[,
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
      data <- mapData()[country==input_country,
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
      data <- mapData()[,
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
      data <- mapData()[country==input_country,
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
    profile_data[, list(youtube_url=paste0("<a href='", youtube_url, "' target='_blank'>Watch Now<a>"),
                        title, speaker, language, upload_time,
                        category, style, taxonomy, human_tags,
                        overall_rating, idea_rating, presentation_rating, video_quality,
                        event, city, country, starts_at, ends_at, twitter_hashtag)]
  }, options=list(columnDefs=list(list(targets=0, searchable=FALSE))))
  
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

