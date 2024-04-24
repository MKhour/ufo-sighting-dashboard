library(shiny)
library(leaflet)
library(dplyr)
library(DT)

#setwd("~/Documents/DS_2003/Final Project/archive (2)")

#ufo_data <- read.csv("ufo_scrubbed.csv")
#setwd('C:/Users/Kyle Tran/Downloads')
#ufo_data = read.csv("ufoData.csv")

#ufo_data <- ufo_data[!ufo_data$state %in% c("", " "), ]

#ufo_data$latitude <- as.numeric(as.character(ufo_data$latitude))
#ufo_data$longitude <- as.numeric(as.character(ufo_data$longitude))

#state_counts <- ufo_data %>%
#  group_by(state) %>%
#  summarise(count = n()) %>%
#  arrange(desc(count))

# Get top 5 states with the most UFO sightings
#top_states <- head(state_counts, 5)

# Sort unique states alphabetically
#unique_states <- sort(unique(ufo_data$state))

# Define UI
ui <- fluidPage(
  titlePanel("UFO Sightings in the United States"),
  HTML("<p>Select a state from the dropdown menu to explore more about UFO sightings reported in that state.</p>"), # New text added
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", choices = c("Please select state", unique_states)),
      h4("Number of Sightings in Selected State:"),
      textOutput("selected_state_count"),
      h4("Top States with Most UFO Sightings:"),
      dataTableOutput("top_states_table")
    ),
    mainPanel(
      leafletOutput("map"),
      HTML("<p><strong>Zoom into the map to select points and learn more about that sighting.</strong></p>") # New caption added
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$state == "Please select state") {
      return(ufo_data)
    } else {
      return(ufo_data[ufo_data$state == input$state, ])
    }
  })
  
  selected_state_count <- reactive({
    nrow(filtered_data())
  })
  
  # Create map
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
  
  # Render top states with most UFO sightings
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

# Run the application
shinyApp(ui = ui, server = server)


