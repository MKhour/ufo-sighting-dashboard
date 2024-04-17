library(shiny)
library(leaflet)
library(dplyr)
library(DT)

# Set the working directory
setwd("~/Documents/DS_2003/Final Project/archive (2)")

# Load UFO sighting data
ufo_data <- read.csv("ufo_scrubbed.csv")

# Remove states with empty labels
ufo_data <- ufo_data[!ufo_data$state %in% c("", " "), ]

# Convert latitude and longitude to numeric
ufo_data$latitude <- as.numeric(as.character(ufo_data$latitude))
ufo_data$longitude <- as.numeric(as.character(ufo_data$longitude))

# Get unique states and their counts
state_counts <- ufo_data %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Get top 5 states with the most UFO sightings
top_states <- head(state_counts, 5)

# Sort unique states alphabetically for dropdown menu
unique_states <- sort(unique(ufo_data$state))

# Define UI
ui <- fluidPage(
  titlePanel("UFO Sightings in the United States"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", choices = unique_states),
      h4("Top States with Most UFO Sightings:"),
      dataTableOutput("top_states_table")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter UFO data based on selected state
  filtered_data <- reactive({
    ufo_data[ufo_data$state == input$state, ]
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addMarkers(data = filtered_data(), 
                 lng = ~longitude, lat = ~latitude,
                 popup = ~as.character(paste("Date:", datetime, "<br>", "City:", city, "<br>", "State:", state)))
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
}

# Run the application
shinyApp(ui = ui, server = server)
