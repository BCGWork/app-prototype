library(shiny)
library(data.table)
library(ggplot2)
library(scales)

shinyServer(function(input, output) {
  catPerfData <- reactive({
    droplevels(category_data[, c("youtube_category", input$xaxis, input$yaxis, input$size, input$color), with=FALSE])
  })
  
  catTopData <- reactive({
    cat_top_data <- droplevels(data[youtube_category==input$cat_options])[order(-rank(views))]
    cat_top_data[, URL:=paste0("https://www.youtube.com/watch?v=", substring(video_id, 2))]
    cat_top_data$upload_date <- format(cat_top_data$upload_date, "%Y-%m-%d")
    cat_top_data
  })
  
  output$cat_perf <- renderPlot({
    cat_perf_data <- catPerfData()
    x_name = input$xaxis
    y_name = input$yaxis
    x_min <- min(cat_perf_data[, x_name, with=FALSE])
    x_max <- max(cat_perf_data[, x_name, with=FALSE])
    y_max <- max(cat_perf_data[, y_name, with=FALSE])
    
    ggplot(cat_perf_data, aes_string(x=x_name, y=y_name, size=input$size, colour=input$color)) +
      geom_point() +
      geom_text(data=cat_perf_data, aes(label=youtube_category), colour="black", size=5) +
      scale_x_continuous(limits=c(-2*x_min, x_max*1.1), labels=comma) +
      scale_y_continuous(limits=c(0, y_max*1.1), labels=comma) +
      scale_color_gradient(low="#f7fcf5", high="#006d2c") +
      scale_size_continuous(range=c(10,30)) +
      labs(x=input$xaxis, y=input$yaxis)
  })
  
  output$cat_top <- renderTable({
    catTopData()[1:input$top_n, c("URL", "youtube_category", "video_detail", "upload_date", names(data)[5:17]), with=FALSE]
  })
  
  output$download_top <- downloadHandler(
    filename <- function() {paste0("top_", input$top_n, "_videos_for_", input$cat_options, ".csv")},
    content <- function(file) {write.csv(catTopData()[1:input$top_n], file, row.names=FALSE)}
  )
})

