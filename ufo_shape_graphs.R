library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

setwd('C:/Users/fletc/Downloads')
ufo_data <- read.csv("scrubbed.csv")
ufo_data <- ufo_data[!ufo_data$state %in% c("", " "), ]

# note changed duration (seconds) to duration
ufo_data$duration <- as.numeric(as.character(ufo_data$duration))
ufo_data$latitude <- as.numeric(as.character(ufo_data$latitude))
ufo_data$longitude <- as.numeric(as.character(ufo_data$longitude))

print(names(ufo_data))


ui <- fluidPage(
  titlePanel("UFO Sightings Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state_select", "Select State", unique(ufo_data$state)),
      numericInput("min_duration", "Minimum Duration (seconds)", min = min(ufo_data$duration, na.rm = TRUE), value = min(ufo_data$duration, na.rm = TRUE)),
      numericInput("max_duration", "Maximum Duration (seconds)", max = max(ufo_data$duration, na.rm = TRUE), value = max(ufo_data$duration, na.rm = TRUE)),
      sliderInput("duration_range", "Duration Range (seconds)", min = min(ufo_data$duration, na.rm = TRUE), max = max(ufo_data$duration, na.rm = TRUE), value = c(min(ufo_data$duration, na.rm = TRUE), max(ufo_data$duration, na.rm = TRUE)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot"))
      )
    )
  )
)

server <- function(input, output) {
  
  observe({
    updateSliderInput(session = getDefaultReactiveDomain(), "duration_range",
                      min = input$min_duration,
                      max = input$max_duration,
                      value = c(input$min_duration, input$max_duration))
  })
  
  filtered_data <- reactive({
    ufo_data %>%
      filter(state %in% input$state_select,
             !is.na(duration),
             duration >= input$duration_range[1],
             duration <= input$duration_range[2])
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = filtered_data(),
                 lng = ufo_data$longitude, lat = ufo_data$latitude,
                 popup = ~paste("Shape:", shape, "<br>", "Duration:", duration, "seconds"),
                 clusterOptions = markerClusterOptions())
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = duration, y = shape, color = shape)) +
      geom_point() +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
