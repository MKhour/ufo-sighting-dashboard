library(shiny)
library(leaflet)
library(lubridate)
library(sf)
library(tigris)
library(plotly)
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


regex <- "&#\\d{1,}"

ufo_data_clean_comments <- ufo_data %>% 
  mutate(
    comments = lapply(comments, 
                      function(og_comment) {
                        gsub(regex, "", og_comment)
                      }),
    year = as.numeric(format(as.Date(datetime, format="%m/%d/%Y %H:%M"),"%Y"))
  )


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

# Get unique states and their counts
state_counts <- ufo_data %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Get top 5 states with the most UFO sightings
top_states <- head(state_counts, 5)

# Sort unique states alphabetically for dropdown menu
unique_states <- sort(unique(ufo_data$state))

###################################################################################################

#2. Creating UI for Page
sightings_by_time_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  titlePanel("UFO Sightings Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(ns("year_range"), "Select Year", min = min(ufo_data$year), max = max(ufo_data$year), value = c(min(ufo_data$year), max(ufo_data$year))),
      checkboxGroupInput(ns("time_of_day"), "Time Of Day", choices = c("Morning"="Morning", "Day"="Day", "Night"="Night"), selected=c("Morning", "Day", "Night")),
      selectInput(ns("states"), "State", choices = c("All Entries", sort(unique(ufo_data$state))))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput(ns("ufo_map_by_time"))),
        tabPanel("Bar Graph", plotlyOutput(ns("ufo_plot_bar_graph")))
      )
    )
  )
)
}


#3. Creating Server for Page
sightings_by_time_server <- function(input, output, session) {
  #a. filtering data
  filtered_data <- reactive({
    req(input$states, input$year_range, input$time_of_day)
    if (input$states == "All Entries") {
      ufo_data %>% filter(year >= input$year_range[1] & 
               year <= input$year_range[2] & time_of_day %in% input$time_of_day) 
    } else {
      ufo_data %>% filter(year >= input$year_range[1] & 
               year <= input$year_range[2] & time_of_day %in% input$time_of_day &
               state == input$states)
    }
  })
  
  #b. graphing bar graph for time
  output$ufo_plot_bar_graph <- renderPlotly({
    data <- filtered_data()
    
    summary_table <- data %>% 
      group_by(year, time_of_day) %>%
      summarize(count = n(), .groups = 'drop')
    
    average_count <- mean(summary_table$count)
    p <- plot_ly(data = summary_table, x=~year, y=~count, type='bar', color = ~time_of_day,
                 colors = c('Morning' = '#FFD700', 'Day' = '#7FFF00', 'Night' = '#1E90FF'),
                 text = ~paste("Count: ", count),
                 hoverinfo = 'text+x+y') %>%
      layout(title ="UFO Sightings Over Time by Year and Time of Day",
             xaxis = list(title="Year"),
             yaxis = list(title="Count"),
             barmode = 'group',
             annotations = list(
               x = 0.5,
               y = -0.15,
               xref = 'paper',
               yref = 'paper',
               text = sprintf("Average count: %.2f", average_count),
               showarrow = FALSE,
               font = list(size=12)
               ),
             margin = list(b=100))
        return(p)
  })

  #c. plotting map
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









