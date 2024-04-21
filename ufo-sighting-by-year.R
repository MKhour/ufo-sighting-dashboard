library(shiny)
library(leaflet)
library(lubridate)
library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
options(tigris_use_cache = TRUE)

#Cleaning the Data for All of App
setwd('C:/Users/Kyle Tran/Downloads')
ufo_data = read.csv("ufoData.csv")
ufo_data$datetime <- as.POSIXct(ufo_data$datetime, format = "%m/%d/%Y %H:%M", errors="coerce")
ufo_data <- ufo_data[!is.na(ufo_data$datetime), ]
ufo_data <- ufo_data[!ufo_data$state %in% c("", " "), ]
ufo_data <- ufo_data %>% rename(duration = duration..seconds.)
ufo_data$shape <- ifelse(ufo_data$shape == "", "unknown", ufo_data$shape)
ufo_data$duration <- as.numeric(as.character(ufo_data$duration))
ufo_data$latitude <- as.numeric(ufo_data$latitude)
ufo_data$longitude <- as.numeric(ufo_data$longitude)

#0 - Extracting Latitude and Longitude and Using Spatial Analysis to Clean Data
#a. making coordinates data frame
latitude = as.numeric(ufo_data$latitude)
longitude = as.numeric(ufo_data$longitude)
coordinates <- data.frame(latitude, longitude)
coordinates <- coordinates %>% 
  filter(!is.na(latitude) & !is.na(longitude))
coordinates <- st_as_sf(coordinates, coords = c("longitude", "latitude"), crs = 4326)

#b. getting state data from package to compare state boundaries
state_data <- states(cb = TRUE, class = "sf")
state_data <- st_transform(state_data, st_crs(coordinates))

#c. comparing the coordinates
results <- st_join(coordinates, state_data, join = st_within)
coordinates <- as.data.frame(st_coordinates(results$geometry))
results <- results %>%
  mutate(latitude = coordinates$Y,
         longitude = coordinates$X)
results <- results %>% 
  select(latitude, longitude, state_abb = STUSPS)

ufo_data <- inner_join(ufo_data, results, by = c("latitude", "longitude")) %>% distinct()
ufo_data$state = toupper(ufo_data$state)
ufo_data <- ufo_data %>% filter(state == state_abb | (country != "us" & country != ""))
View(ufo_data)

#Extracting Years/Times From DateTime
ufo_data$year <- year(ufo_data$datetime)
ufo_data$time_of_day <- case_when(
  hour(ufo_data$datetime) < 12 ~ "Morning",
  hour(ufo_data$datetime) < 18 ~ "Day",
  TRUE ~ "Night"
)


#2. Creating UI for Page
sightings_by_time_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  titlePanel("UFO Sightings Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(ns("year_range"), "Select Year", min = min(ufo_data$year), max = max(ufo_data$year), value = c(min(ufo_data$year), max(ufo_data$year))),
      selectInput(ns("time_of_day"), "Select Time of Day", choices = c("Morning", "Day", "Night")),
      selectInput(ns("states"), "State", choices = c("All Entries", sort(unique(ufo_data$state))))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput(ns("ufo_map_by_time"))),
        tabPanel("Bar Graph", plotOutput(ns("ufo_plot_bar_graph")))
      )
    )
  )
)
}


#3. Creating Server for Page
sightings_by_time_server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$states, input$year_range, input$time_of_day)
    if (input$states == "All Entries") {
      ufo_data %>% filter(year >= input$year_range[1] & 
               year <= input$year_range[2] & time_of_day == input$time_of_day) 
    } else {
      ufo_data %>% filter(year >= input$year_range[1] & 
               year <= input$year_range[2] & time_of_day == input$time_of_day &
               state == input$states)
    }
  })
  
  output$ufo_plot_bar_graph <- renderPlot({
    ggplot(data = filtered_data(), aes(x = year)) +
      geom_bar() +
      labs(title = "UFO Sightings Over Time by Year and Time of Day",
           x = "Year", y = "Count") +
      theme_minimal() 
  })

 output$ufo_map_by_time <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      setView(lng = -96.7970, lat = 32.7767, zoom = 4)
   })
 
 #Using leafletProxy to update map without re-rendering
 observe({
   req(input$states, input$year_range, input$time_of_day)
   data <- filtered_data()
   
   leafletProxy("ufo_map_by_time", data = data) %>% 
     clearMarkerClusters() %>%
     clearMarkers() %>%
     addMarkers(data = filtered_data(), lat = ~as.numeric(latitude), 
                lng = ~as.numeric(longitude), 
                popup = ~paste(city, state, datetime),
                clusterOptions = markerClusterOptions())
 })
  
}

shinyApp(ui = sightings_by_time_ui, server = sightings_by_time_server)









