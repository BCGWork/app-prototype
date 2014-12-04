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


options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
load("credential/twitter.RData")
registerTwitterOAuth(twitCred)

shinyServer(function(input, output) {
  ###########################
  #### reactive data listener
  periodData <- reactive({
    cockpit_period <- input$cockpit_period
    
    output <- profile_data[
      (starts_at>=cockpit_period[1] & ends_at<=cockpit_period[2])
      ]
    output    
  })
  
  tagDynamicsData <- reactive({
    tagDType <- input$tagD_type
    tagDCountry <- input$tagD_country
    tagDContent <- input$tagD_content
    tagDFormat <- input$tagD_format
    tagDIntent <- input$tagD_intent
    
    output <- profile_data[
      (country==tagDCountry | tagDCountry=="All") &
        (is.null(tagDContent) | Content_tag1 %in% tagDContent | Content_tag2 %in% tagDContent | Content_tag3 %in% tagDContent | Content_tag4 %in% tagDContent | Content_tag5 %in% tagDContent) &
        (is.null(tagDFormat) | Format_tag1 %in% tagDFormat | Format_tag2 %in% tagDFormat | Format_tag3 %in% tagDFormat) &
        (is.null(tagDIntent) | Intent_tag1 %in% tagDIntent | Intent_tag2 %in% tagDIntent)
      ]
    output
  })
  
  tagNetworkData <- reactive({
    tagType <- input$tag_type
    tagCountry <- input$tag_country
    tagPeriod <- input$tag_period
    
    if (tagType=="Topic") {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5)
        ]
      output <- f_out
    } else if (tagType=="Delivery Format") {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Format_tag1, Format_tag2, Format_tag3)
        ]
      output <- f_out
    } else {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Intent_tag1, Intent_tag2)
        ]
      output <- f_out
    }
    output
  })
  
  tagComparativeNetworkData <- reactive({
    tagType <- input$tag_type
    tagCountry <- input$tag_country
    tagPeriod <- input$tag_comp_period
    
    if (tagType=="Topic") {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5)
        ]
      output <- f_out
    } else if (tagType=="Delivery Format") {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Format_tag1, Format_tag2, Format_tag3)
        ]
      output <- f_out
    } else {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Intent_tag1, Intent_tag2)
        ]
      output <- f_out
    }
    output
  })
  
  tagNetworkVis <- reactive({
    tagType <- input$tagN_type
    tagCountry <- input$tagN_country
    tagPeriod <- input$tagN_period
    
    if (tagType=="Topic") {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5)
        ]
      output <- f_out
    } else if (tagType=="Delivery Format") {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Format_tag1, Format_tag2, Format_tag3)
        ]
      output <- f_out
    } else {
      f_out <- profile_data[
        (country==tagCountry | tagCountry=="All") & (starts_at>=tagPeriod[1] & ends_at<=tagPeriod[2]),
        list(video_id, Intent_tag1, Intent_tag2)
        ]
      output <- f_out
    }
    output
  })
  
  tagEvolutionData <- reactive({tagCombinationsEvol(tagNetworkData(), tagComparativeNetworkData(), input$tag_number)})
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
  output$last_events <- renderDataTable({
    cockpit_country <- input$cockpit_country
    data_country <- periodData()[(country==cockpit_country | cockpit_country=="All")]
    # Select last events
    last_events <- data_country[, list(talks=length(unique(video_id)), views=sum(views), likes=sum(likes), dislikes=sum(dislikes), shares=sum(shares)), by=list(event,starts_at,ends_at)][order(starts_at,decreasing=TRUE)][1:5, ]
    last_events
  }, options = list(paging=FALSE, searching=FALSE))
  
  output$cockpit_tags <- renderDataTable({
    cockpit_country <- input$cockpit_country
    comb <- input$cockpit_tagcomb
    data_ww <- periodData()[, list(video_id, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5)]
    data_country <- periodData()[(country==cockpit_country | cockpit_country=="All"), list(video_id, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5)]
    
    nodes_ww <- as.data.table(processTagComb(data_ww, comb))
    nodes_ww$rank <- rank(-nodes_ww$count, ties.method="min")
    nodes_country <- as.data.table(processTagComb(data_country, comb))
    nodes_country$rank <- rank(-nodes_country$count, ties.method="min")
    setkey(nodes_ww, tag)
    setkey(nodes_country, tag)
    setnames(nodes_ww, "rank", "rank_ww")
    nodes_country <- merge(nodes_country, nodes_ww[, list(tag, rank_ww)], all.x=TRUE)
    nodes_country$diff <- nodes_country$rank - nodes_country$rank_ww
    
    nodes_country[order(rank), list(tag, count, rank, rank_ww, diff)][1:20,]
  }, options = list(paging=FALSE, searching=FALSE,
                    rowCallback = I(
                      'function(row, data) {
                      // Bold cells for those > 0 in the first column
                      if (parseFloat(data[4]) > 0.0)
                        $("td", row).css("background-color", "#FFCCCC");
                      if (parseFloat(data[4]) < 0.0)
                        $("td", row).css("background-color", "#E5FFCC");
                      }'
                    ))
  )
  
  
  output$tagDynamics <- renderPlot({
    tagDType <- input$tagD_type
    if (tagDType=="Topic") {
      data <- tagDynamicsData()[, list(video_id, event_year, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5)]
      tagD <- input$tagD_content
    } else if (tagDType=="Delivery Format") {
      data <- tagDynamicsData()[, list(video_id, event_year, Format_tag1, Format_tag2, Format_tag3)]
      tagD <- input$tagD_format
    } else {
      data <- tagDynamicsData()[, list(video_id, event_year, Intent_tag1, Intent_tag2)]
      tagD <- input$tagD_intent
    }
    meltData <- melt(data, id.vars=c("video_id", "event_year"))
    setnames(meltData, "value", "tags")
    meltData <- meltData[!is.na(tags) & tags!="" & !is.na(event_year)]
    plotData <- meltData[is.null(tagD) | tags %in% tagD, list(talks=length(unique(video_id))), by=list(tags, event_year)]
    if (length(unique(plotData$tags)) > 9) {
      topCat <- plotData[, list(talks=sum(talks)), by=tags][order(-rank(talks))][1:9, tags]
      plotData <- plotData[tags %in% topCat]
    }
    
    ggplot(plotData, aes_string(x="event_year", y="talks", colour="tags")) +
      geom_line(size=1) +
      scale_y_continuous(labels=comma) +
      scale_color_brewer(palette="Set1") +
      xlab("event year") +
      theme(
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15, vjust=1)
      )
  }, width=1024)
  
  output$tagTEDxvsTED <- renderPlot({
    tag_TEDvsTEDx <- input$tag_TEDvsTEDx
    TEDx <- profile_data[
      (Content_tag1 %in% tag_TEDvsTEDx | Content_tag2 %in% tag_TEDvsTEDx | Content_tag3 %in% tag_TEDvsTEDx | Content_tag4 %in% tag_TEDvsTEDx | Content_tag5 %in% tag_TEDvsTEDx),
      list(video_id, event_year, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5)
      ]
    meltData <- melt(TEDx, id.vars=c("video_id", "event_year"))
    setnames(meltData, "value", "tags")
    meltData <- meltData[!is.na(tags) & tags!="" & !is.na(event_year)]
    plotData_TEDx <- meltData[tags %in% tag_TEDvsTEDx, list(talks=length(unique(video_id))), by=list(tags, event_year)]
    plotData_TEDx$type = "TEDx"
    
    TED <- ted_data[
      (tolower(Tag) %in% tolower(tag_TEDvsTEDx)), 
      list(TalkId, Tag, event_year)
      ]
    setnames(TED, "Tag", "tags")
    plotData_TED <- TED[, list(talks=length(unique(TalkId))), by=list(tags, event_year)]
    plotData_TED$type = "TED"
    plotData <- rbind(plotData_TEDx, plotData_TED)
    
    ggplot(plotData, aes_string(x="event_year", y="talks", colour="type")) +
      geom_point() + geom_line() +
      scale_y_continuous(labels=comma) +
      scale_color_brewer(palette="Set1") +
      xlab("event year") +
      theme(
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15, vjust=1)
      )
  }, width=1024)
  
  output$mostUsedTags <- renderDataTable({
    data <- tagNetworkData()
    nodes <- as.data.frame(processTagComb(data, input$tag_number))
    nodes[order(nodes$count, decreasing=TRUE), c("tag", "count")]
  })
  
  output$tagEvol <- renderDataTable({tagEvolutionData()})
  
  output$tagNew <- renderDataTable({tagEvolutionData()[status=="new tag"]})
  
  output$tagDis <- renderDataTable({tagEvolutionData()[status=="disappeared tag"]})
  
  output$tagNetwork <- renderPrint({
    data <- tagNetworkVis()
    data <- processTag(data)
    adj_mat <- data$adj_mat
    links <- data$edge_list
    nodes <- data$node_list
    tagCluster <- input$tag_cluster
    cl <- kmeans(adj_mat, tagCluster, iter.max=100, nstart=30)$cluster
    cl_dt <- data.table(ID=as.numeric(names(cl)), group=cl)
    setkey(nodes, ID)
    setkey(cl_dt, ID)
    nodes <- merge(nodes, cl_dt)
    d3ForceNetworkAdapted(Links=links, Node=as.data.frame(nodes), Source="from", Target="to",
                          Value="weight", NodeID="tag", Group="group", Count="count", width=1024, height=768,
                          opacity=1, standAlone=FALSE, parentElement="#tagNetwork", zoom=TRUE, showText=input$tag_showlabel,
                          charge=-200, fontsize=6)
    
  }, width=1024)
  
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
                        category, style, Content_tag1, Content_tag2, Content_tag3, Content_tag4, Content_tag5,
                        Format_tag1, Format_tag2, Format_tag3, Intent_tag1, Intent_tag2,
                        overall_rating, idea_rating, presentation_rating, video_quality,
                        event, city, country, starts_at, ends_at, twitter_hashtag)]
  }, options=list(columnDefs=list(list(targets=0, searchable=FALSE))))
  
  output$word_cloud <- renderPlot({
    input$search_tweets
    if (input$search_tweets == 0) {
      return()
    }
    isolate(wordcloud(tweetWords()$word, tweetWords()$freq, scale=c(8, 0.3), min.freq=3, random.order=FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2")))
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

