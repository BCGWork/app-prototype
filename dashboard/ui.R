library(shiny)
# hashtag <- unique(profile_data$twitter_hashtag)[nchar(unique(profile_data$twitter_hashtag))>0]
hashtag <- unique(profile_data$event)

shinyUI(
  navbarPage(
    "TED Dashboard",
    tabPanel(
      "Tag Analyses",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "tag_type",
            label="Type of tags",
            choices=c("Topic", "Delivery Format", "Speaker Intent"),
            selected="Topic"
          ),
          br(),
          selectInput(
            "tag_country",
            label="Country",
            choices=c("All", unique(profile_data$country)[order(unique(profile_data$country))]),
            selected="All"
          ),
          br(),
          dateRangeInput("tag_period", label="Period", start=max(profile_data$ends_at, na.rm=TRUE)-180, end=max(profile_data$ends_at, na.rm=TRUE))
        ),
        mainPanel(
          tabsetPanel(
            #             sliderInput("tag_number", label="# of tag in combination", min=1, max=5, value=2)
            #             dateRangeInput("tag_comp_period", label="Comparison period", start=as.Date(paste0(min(profile_data$event_year, na.rm=TRUE),"-01-01")), end=as.Date(paste0(min(profile_data$event_year, na.rm=TRUE),"-12-31")))
            tabPanel("Frequent Tags", dataTableOutput("mostUsedTags")),
            tabPanel("Tag Evolution", dataTableOutput("tagEvol")),
            tabPanel("New Tags", dataTableOutput("tagNew")),
            tabPanel("Disappeared Tags", dataTableOutput("tagDis")),
            tabPanel(
              "Tag Network",
              sliderInput("tag_cluster", label="Number of tag groups", min=1, max=15, step=1, value=5),
              htmlOutput("tagNetwork")
            )
          )
        )
      )
    ),
    tabPanel(
      "Rating & Popularity",
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
          dateRangeInput(
            "filter_period",
            label="Filter by Period",
            start=min(profile_data$starts_at, na.rm=TRUE),
            end=max(profile_data$ends_at, na.rm=TRUE)
          ),
          br(),
          selectInput(
            "filter_tag",
            label="Filter by Content Tag",
            choices=content_tags,
            multiple=TRUE,
            selectize=TRUE
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
      "Geographical View",
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
          dateRangeInput(
            "map_period",
            label="Filter by Period",
            start=min(profile_data$starts_at, na.rm=TRUE),
            end=max(profile_data$ends_at, na.rm=TRUE)
          ),
          br(),
          selectInput(
            "map_tag",
            label="Filter by Content Tag",
            choices=content_tags,
            multiple=TRUE,
            selectize=TRUE
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
    tabPanel("Related Talks", dataTableOutput("search_output")),
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
    )
  )
)


