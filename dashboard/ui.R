library(shiny)
hashtag <- unique(profile_data$twitter_hashtag)[nchar(unique(profile_data$twitter_hashtag))>0]

shinyUI(
  navbarPage(
    "TED Dashboard",
    tabPanel(
      "General Profile",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "xaxis",
            label="Ratings",
            choices=c("overall_rating", "idea_rating", "presentation_rating", "video_quality"),
            selected="overall_rating"
          ),
          br(),
          selectInput(
            "yaxis",
            label="Performance Metrics",
            choices=c("talks", "views", "shares", "estimatedMinutesWatched", "averageViewPercentage", "subscribersGained"),
            selected="talks"
          ),
          br(),
          selectInput(
            "filter_category",
            label="Filter by Talk Category",
            choices=c("All", unique(profile_data$category)[order(unique(profile_data$category))]),
            selected="All"
          ),
          br(),
          selectInput(
            "filter_style",
            label="Filter by Talk Style",
            choices=c("All", unique(profile_data$style)[order(unique(profile_data$style))]),
            selected="All"
          ),
          br(),
          selectInput(
            "filter_country",
            label="Filter by Event Country",
            choices=c("All", unique(profile_data$country)[order(unique(profile_data$country))]),
            selected="All"
          ),
          br(),
          selectInput(
            "filter_year",
            label="Filter by Event Year",
            choices=c("All", min(profile_data$event_year, na.rm=TRUE):max(profile_data$event_year, na.rm=TRUE)),
            selected="All"
          ),
          br(),
          selectInput(
            "filter_language",
            label="Filter by Talk Language",
            choices=c("All", unique(profile_data$language)[order(unique(profile_data$language))]),
            selected="All"
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Overview", plotOutput("overview_plot", height="768px")),
            tabPanel("View by Category", plotOutput("category_plot", height="768px")),
            tabPanel("View by Style", plotOutput("style_plot", height="768px"))
          )
        )
      )
    ),
    tabPanel(
      "Geographical Profile",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "map_circle",
            label="Size of Circle (Performance Metrics)",
            choices=c("talks", "views", "shares", "estimatedMinutesWatched", "averageViewPercentage", "subscribersGained"),
            selected="talks"
          ),
          br(),
          selectInput(
            "map_country",
            label="Country",
            choices=c("World", unique(profile_data$country)[order(unique(profile_data$country))]),
            selected="World"
          ),
          br(),
          sliderInput("zoom_scale", label="Zoom Scale", min=1, max=11, value=4),
          br(),
          sliderInput("map_bubble_size", label="Bubble Size", min=3, max=30, value=c(5,15))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Map Overview", plotOutput("map_plot", height="768px")),
            tabPanel("Maps by Category", plotOutput("map_cat_plot", height="768px")),
            tabPanel("Maps by Style", plotOutput("map_style_plot", height="768px"))
          )
        )
      )
    ),
    tabPanel(
      "Pattern Analysis"
    ),
    tabPanel(
      "Social Media Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "twitter_hashtag",
            label="TEDx #hashtag",
            choices=c("#TEDxTalks", hashtag[order(hashtag)]),
            selected="#TEDxTalks"
          ),
          dateRangeInput("twitter_period", label="between", start=Sys.Date()-10, end=Sys.Date()),
          br(),
          actionButton("search_tweets", label="Search")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Word Cloud", plotOutput("word_cloud", height="768px")),
            tabPanel("Tweets", dataTableOutput("tweets")),
            tabPanel(
              "Interaction by Time",
              selectInput(
                "twitter_ts_y",
                label="Measure of interaction",
                choices=c("tweets", "sentiment", "placeholder"),
                selected="tweets"
              ),
              plotOutput("twitter_ts", height="768px")
            ),
            tabPanel(
              "Event Location Trend",
              selectInput(
                "trend_location",
                label="Select event location",
                choices=unique(profile_data$event),
                selected="TEDxTimeSquare"
              ),
              dataTableOutput("loc_trend")
            )
          )
        )
      )
    ),
    tabPanel("TEDx Search", dataTableOutput("search_output"))
  )
)


