library(shiny)

shinyUI(
  navbarPage("TED Dashboard",
             tabPanel("General Profile",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("xaxis",
                                      label="X Axis",
                                      choices=names(category_data)[-1],
                                      selected="videos"
                          ),
                          br(),
                          selectInput("yaxis",
                                      label="Y Axis",
                                      choices=names(category_data)[-1],
                                      selected="viewsPerVideo"
                          ),
                          br(),
                          selectInput("size",
                                      label="Size",
                                      choices=names(category_data)[-1],
                                      selected="minutesWatchedPerDay"
                          ),
                          br(),
                          selectInput("color",
                                      label="Color",
                                      choices=names(category_data)[-1],
                                      selected="viewsPerVideo"
                          ),
                          br(),
                          selectInput("cat_options",
                                      label="Deep dive into category ... ",
                                      choices=unique(category_data$youtube_category),
                                      selected="Education"
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(
                              "Category Performance",
                              downloadButton("download_perf", "Download Data"),
                              br(),
                              br(),
                              br(),
                              plotOutput("cat_perf", height="800px")
                            ),
                            tabPanel(
                              "Top Categorical Videos",
                              downloadButton("download_top", "Download Data"),
                              br(),
                              br(),
                              br(),
                              sliderInput("top_n", label="number of videos to show", min=1, max=20, value=10),
                              br(),
                              tableOutput("cat_top")
                            )
                          )
                        )
                      )
             ),
             tabPanel("Geographical Footprint"),
             tabPanel("Temporal Activities"),
             tabPanel("Placeholder")
  )
)

