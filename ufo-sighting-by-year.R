library(shiny)
library(leaflet)
library(lubridate)
library(dplyr)
library(ggplot2)

setwd('C:/Users/Kyle Tran/Downloads')
getwd()
ufo_data = read.csv("ufoData.csv")
ufo_data$datetime <- as.POSIXct(ufo_data$datetime, format = "%m/%d/%Y %H:%M", errors="coerce")
ufo_data <- ufo_data[!is.na(ufo_data$datetime), ]

 
#1. Extracting Years/Times From DateTime
ufo_data$year <- year(ufo_data$datetime)
ufo_data$time_of_day <- case_when(
  hour(ufo_data$datetime) < 12 ~ "Morning",
  hour(ufo_data$datetime) < 18 ~ "Day",
  TRUE ~ "Night"
)

#2. Creating UI for Page
sightings_by_time_ui <- fluidPage(
  titlePanel("UFO Sightings Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year", min = min(ufo_data$year), max = max(ufo_data$year), value = c(min(ufo_data$year), max(ufo_data$year))),
      selectInput("time_of_day", "Select Time of Day", choices = c("Morning", "Day", "Night")),
      selectInput("viz_type", "Select Visualization Type", choices = c("Map", "Bar Graph"), selected ="Bar Graph"),
    ),
  
    mainPanel(
      uiOutput("visual")
    )
  )
)

#3. Creating Server for Page
sightings_by_time_server <- function(input, output) {
  filtered_data <- reactive({
    filter(ufo_data, year >= input$year_range[1] & 
      year <= input$year_range[2] & time_of_day == input$time_of_day)
  })
  
  output$visual <- renderUI({
    if (input$viz_type == "Bar Graph") {
        plotOutput("ufo_plot")
    } else if (input$viz_type == "Map") {
      leafletOutput("map")
    }
  })
  
  output$ufo_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = year)) +
      geom_bar() +
      labs(title = "UFO Sightings Over Time by Year and Time of Day",
           x = "Year", y = "Count") +
      theme_minimal() 
  })

 output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = filtered_data(), lat = ~as.numeric(latitude), 
                 lng = ~as.numeric(longitude))
   })
  
}

shinyApp(ui = sightings_by_time_ui, server = sightings_by_time_server)









