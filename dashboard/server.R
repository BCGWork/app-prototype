library(shiny)
library(data.table)
library(ggplot2)
library(scales)
library(ggmap)

shinyServer(function(input, output) {
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
  })
  
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
      geom_text(data=data, aes_string(label=input_y, vjust=-0.1), size=3) +
      scale_y_continuous(labels=comma) +
      facet_wrap(~category, scales="free_y") +
      theme(
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15, vjust=1),
        strip.text.x=element_text(size=15)
      )
  })
  
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
      geom_text(data=data, aes_string(label=input_y, vjust=-0.1), size=3) +
      scale_y_continuous(labels=comma) +
      facet_wrap(~style, scales="free_y") +
      theme(
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15, vjust=1),
        strip.text.x=element_text(size=15)
      )
  })
  
  output$map_plot <- renderPlot({
    input_mapCircle <- input$map_circle
    input_country <- input$map_country
    input_zoom <- input$zoom_scale
    if (input_country == "World") {
      data <- overviewData()[,
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
        scale_size_continuous(range=c(5, 15)) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data)
    } else {
      data <- overviewData()[country==input_country,
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
        scale_size_continuous(range=c(10, 30)) +
        geom_point(aes(x=lng, y=lat), size=2, color="#2ca25f", data=data)
    }
  })
  
  output$map_cat_plot <- renderPlot({
    input_mapCircle <- input$map_circle
    input_country <- input$map_country
    input_zoom <- input$zoom_scale
    if (input_country == "World") {
      data <- overviewData()[,
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
        scale_size_continuous(range=c(5, 15)) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data) +
        facet_wrap(~category) +
        theme(strip.text.x=element_text(size=15))
    } else {
      data <- overviewData()[country==input_country,
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
        scale_size_continuous(range=c(5, 15)) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data) +
        facet_wrap(~category) +
        theme(strip.text.x=element_text(size=15))
    }
  })
  
  output$map_style_plot <- renderPlot({
    input_mapCircle <- input$map_circle
    input_country <- input$map_country
    input_zoom <- input$zoom_scale
    if (input_country == "World") {
      data <- overviewData()[,
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
        scale_size_continuous(range=c(5, 15)) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data) +
        facet_wrap(~style) +
        theme(strip.text.x=element_text(size=15))
    } else {
      data <- overviewData()[country==input_country,
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
        scale_size_continuous(range=c(5, 15)) +
        geom_point(aes(x=lng, y=lat), size=1, color="#2ca25f", data=data) +
        facet_wrap(~style) +
        theme(strip.text.x=element_text(size=15))
    }
  })
  
  
})


# catPerfData <- reactive({
#   droplevels(category_data[, c("youtube_category", input$xaxis, input$yaxis, input$size, input$color), with=FALSE])
# })
# 
# catTopData <- reactive({
#   cat_top_data <- droplevels(perf_data[youtube_category==input$cat_options])[order(-rank(views))]
#   cat_top_data[, URL:=paste0("https://www.youtube.com/watch?v=", substring(video_id, 2))]
#   cat_top_data$upload_date <- format(cat_top_data$upload_date, "%Y-%m-%d")
#   cat_top_data
# })
# 
# output$cat_perf <- renderPlot({
#   cat_perf_data <- catPerfData()
#   x_name = input$xaxis
#   y_name = input$yaxis
#   x_min <- min(cat_perf_data[, x_name, with=FALSE])
#   x_max <- max(cat_perf_data[, x_name, with=FALSE])
#   y_max <- max(cat_perf_data[, y_name, with=FALSE])
#   
#   ggplot(cat_perf_data, aes_string(x=x_name, y=y_name, size=input$size, colour=input$color)) +
#     geom_point() +
#     geom_text(data=cat_perf_data, aes(label=youtube_category), colour="black", size=5) +
#     scale_x_continuous(limits=c(-2*x_min, x_max*1.1), labels=comma) +
#     scale_y_continuous(limits=c(0, y_max*1.1), labels=comma) +
#     scale_color_gradient(low="#f7fcf5", high="#006d2c") +
#     scale_size_continuous(range=c(10,30)) +
#     labs(x=input$xaxis, y=input$yaxis)
# })
# 
# output$download_perf <- downloadHandler(
#   filename <- function() {"category_performance.csv"},
#   content <- function(file) {write.csv(catPerfData(), file, row.names=FALSE)}
# )
# 
# output$cat_top <- renderTable({
#   catTopData()[1:input$top_n, c("URL", "youtube_category", "video_detail", "upload_date", names(perf_data)[5:17]), with=FALSE]
# })
# 
# output$download_top <- downloadHandler(
#   filename <- function() {paste0("top_", input$top_n, "_videos_for_", input$cat_options, ".csv")},
#   content <- function(file) {write.csv(catTopData()[1:input$top_n], file, row.names=FALSE)}
# )

