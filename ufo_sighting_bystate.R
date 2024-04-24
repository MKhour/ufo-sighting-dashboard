library(shiny)
library(leaflet)
library(dplyr)
library(DT)

# Set the working directory
#setwd("~/Documents/DS_2003/Final Project/archive (2)")

# Load UFO sighting data
#ufo_data <- read.csv("ufo_scrubbed.csv")
#setwd('C:/Users/Kyle Tran/Downloads')
#ufo_data = read.csv("ufoData.csv")

# Remove states with empty labels
#ufo_data <- ufo_data[!ufo_data$state %in% c("", " "), ]

# Convert latitude and longitude to numeric
#ufo_data$latitude <- as.numeric(as.character(ufo_data$latitude))
#ufo_data$longitude <- as.numeric(as.character(ufo_data$longitude))

# Get unique states and their counts
#state_counts <- ufo_data %>%
#  group_by(state) %>%
#  summarise(count = n()) %>%
#  arrange(desc(count))

# Get top 5 states with the most UFO sightings
#top_states <- head(state_counts, 5)

# Sort unique states alphabetically for dropdown menu
#unique_states <- sort(unique(ufo_data$state))

# Define UI
state_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  titlePanel("UFO Sightings in the United States"),
  p("On this page, we focused our analysis on the frequency of UFO sightings across the 50 states. As
    you will see below, we have an interactive map that allows you to see
    the UFO sightings across different states. Note, when you zoom in close enough,
    you can click on a point to learn more about a specifc datapoint."),
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("state"), "Select State:", choices = c("Please select state", unique_states)),
      h4("Number of Sightings in Selected State:"),
      textOutput(ns("selected_state_count")),
      h4("Top States with Most UFO Sightings:"),
      dataTableOutput(ns("top_states_table"))
    ),
    mainPanel(
      leafletOutput(ns("map"))
    )
  )
)}

# Define server logic
state_server <- function(input, output, session) {
  
  # Filter UFO data based on selected state
  filtered_data <- reactive({
    if (input$state == "Please select state") {
      return(ufo_data)
    } else {
      return(ufo_data[ufo_data$state == input$state, ])
    }
  })
  
  # Number of sightings for selected state
  selected_state_count <- reactive({
    nrow(filtered_data())
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addCircleMarkers(data = filtered_data(), 
                       radius = 3,
                       clusterOptions = markerClusterOptions(), # Cluster markers by city
                       popup = ~as.character(paste("Date:", datetime, "<br>", 
                                                   "City:", city, "<br>", 
                                                   "State:", state, "<br>",
                                                   "Comments:", comments)))
  })
  
  # Render top states with most UFO sightings as a datatable
  output$top_states_table <- renderDataTable({
    datatable(top_states, 
              options = list(
                paging = FALSE,
                searching = FALSE,
                info = FALSE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              )
    )
  })
  
  # Render number of sightings in selected state
  output$selected_state_count <- renderText({
    if (input$state == "Please select state") {
      return("Please select a state.")
    } else {
      paste("Total sightings in", input$state, ":", selected_state_count())
    }
  })
}

#ui <- fluidPage(
#  state_ui("1")
#)

#server <- function(input, output, session) {
#  callModule(state_server, "1")
#}

# Run the application
#shinyApp(ui = ui, server = server)


