library(shiny)

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
            tabPanel(
              "Overview",
              plotOutput("overview_plot", height="600px")
            ),
            tabPanel(
              "View by Category",
              plotOutput("category_plot", height="600px")
            ),
            tabPanel(
              "View by Style",
              plotOutput("style_plot", height="600px")
            )
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
          sliderInput(
            "zoom_scale",
            label="Zoom Scale",
            min=1,
            max=11,
            value=4
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Map Overview",
              plotOutput("map_plot", height="600px")
            ),
            tabPanel(
              "Maps by Category",
              plotOutput("map_cat_plot", height="600px")
            ),
            tabPanel(
              "Maps by Style",
              plotOutput("map_style_plot", height="600px")
            )
          )
        )
      )
    ),
    tabPanel(
      "Pattern Analysis"
    ),
    tabPanel(
      "Social Media Analysis"
    )
  )
)



# tabPanel(
#   "General Profile",
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("xaxis",
#                   label="X Axis",
#                   choices=names(category_data)[-1],
#                   selected="videos"
#       ),
#       br(),
#       selectInput("yaxis",
#                   label="Y Axis",
#                   choices=names(category_data)[-1],
#                   selected="viewsPerVideo"
#       ),
#       br(),
#       selectInput("size",
#                   label="Size",
#                   choices=names(category_data)[-1],
#                   selected="minutesWatchedPerDay"
#       ),
#       br(),
#       selectInput("color",
#                   label="Color",
#                   choices=names(category_data)[-1],
#                   selected="viewsPerVideo"
#       )
#     ),
#     mainPanel(
#       tabsetPanel(
#         tabPanel(
#           "Category Performance",
#           downloadButton("download_perf", "Download Data"),
#           br(),
#           br(),
#           br(),
#           plotOutput("cat_perf", height="800px")
#         ),
#         tabPanel(
#           "Top Categorical Videos",
#           downloadButton("download_top", "Download Data"),
#           br(),
#           br(),
#           br(),
#           selectInput("cat_options",
#                       label="select a category",
#                       choices=unique(category_data$youtube_category),
#                       selected="Education"
#           ),
#           sliderInput("top_n", label="number of videos to show", min=1, max=20, value=10),
#           br(),
#           tableOutput("cat_top")
#         )
#       )
#     )
#   )
# )