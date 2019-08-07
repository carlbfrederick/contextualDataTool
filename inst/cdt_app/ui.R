library(shinydashboard)
library(leaflet)
library(mapview)
library(purrr)
library(dplyr)
library(sf)
library(glue)
library(shinycssloaders)
library(shinyWidgets)

dashboardPage(title = "Contextual Data Tool",
  #dashboardHeader(title = "Contextual Data Tool", titleWidth = "250"),
  dashboardHeader(title = tags$a(href='https://dpi.wi.gov',
                                 tags$img(src = 'images/dpi_logo_horiz.png',
                                          height = "45")), titleWidth = "250"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      column(width = 3,
             box(title = "Contextual Data", width = 12,
                 h3("Step 1:"),
                 selectizeInput("topic", label = "Choose a Topic",
                                choices = c("Computers & Internet at Home (0-18 year-olds)" = "it",
                                            "Education Level (18-24 year-olds)" = "ed",
                                            "Family Structure" = "fs",
                                            "Health Insurance Coverage (0-18 year-olds)" = "hi",
                                            "Housing Unit Status" = "hh",
                                            "Labor Force Participation" = "lf",
                                            "Language at Home (5-17 year-olds)" = "la",
                                            "Place of Birth (5-17 year-olds)" = "pb",
                                            "Residential Mobility (5-17 year-olds)" = "rm",
                                            "Travel Time to Work" = "dr")),
                 uiOutput("topicvar"),
                 actionButton("makemap", "Map it!"),
                 h3("Step 2:"),
                 switchInput("distType", label = "District Type", onLabel = "Elementary", offLabel = "Secondary"),
                 uiOutput("distList"),
                 actionButton("zoomin", label = "Zoom to District"),
                 actionButton("zoomout", label = "Zoom to State"))
             ),
      column(width = 6,
             box(title = "Map", width = 12,
                 # downloadButton("downloadMap", "Download Map"),
                 withSpinner(leafletOutput("dispMap", height = 600))
             ),
             box(title = "Legend", width = 12, height = 160,
                 plotOutput("custom_legend")
             )
      ),
      column(width = 3,
             box(title = "Education Data", width = 12,
                 textOutput("distScore")))
    )
  ),
  skin = "black"
)
