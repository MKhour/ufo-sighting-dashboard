# library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)

#setwd('C:/Users/fletc/Downloads')
#setwd('C:/Users/Kyle Tran/Downloads')
#ufo_data = read.csv("ufoData.csv")
#ufo_data <- read.csv("scrubbed.csv")
#ufo_data <- ufo_data[!ufo_data$state %in% c("", " "), ]
#ufo_data <- ufo_data %>% rename(duration = duration..seconds.)
#ufo_data$shape <- ifelse(ufo_data$shape == "", "unknown", ufo_data$shape)

# note changed duration (seconds) to duration
#ufo_data$duration <- as.numeric(as.character(ufo_data$duration))
#ufo_data$latitude <- as.numeric(as.character(ufo_data$latitude))
#ufo_data$longitude <- as.numeric(as.character(ufo_data$longitude))

#print(names(ufo_data$shapes))


shape_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  titlePanel("UFO Sightings By Shape"),
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("state_select"), "State", unique(ufo_data$state)),
      numericInput(ns("min_duration"), "Minimum Duration (seconds)", min = min(ufo_data$duration, na.rm = TRUE), value = min(ufo_data$duration, na.rm = TRUE)),
      numericInput(ns("max_duration"), "Maximum Duration (seconds)", min = min(ufo_data$duration, na.rm = TRUE), value = max(ufo_data$duration, na.rm = TRUE)),
      selectInput(ns("shape_select"), "Select Shape", choices = unique(ufo_data$shape)),
      checkboxInput(ns("show_combined"), "Show Total Shape Counts", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot", plotOutput(ns("scatter_plot"))),
        tabPanel("Barchart", plotlyOutput(ns("shape_plot"))),
        tabPanel("Map", leafletOutput(ns("map"))
      )
    )
  ))
)}

shape_server <- function(input, output, session) {
  filtered_data <- reactive({
    ufo_data %>%
      filter(state %in% input$state_select,
             !is.na(duration),
             duration >= input$min_duration,
             duration <= input$max_duration)
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
  
  output$shape_plot <- renderPlotly({
    filtered_data <- ufo_data %>%
      filter(shape == input$shape_select)
    
    if (input$show_combined) {
      other_data <- ufo_data %>%
        filter(shape != input$shape_select)
      
      bar_data <- other_data %>%
        group_by(state) %>%
        summarise(other_count = n()) %>%
        left_join(filtered_data %>%
                    group_by(state) %>%
                    summarise(selected_count = n()), by = "state")
      
      p <- ggplot(bar_data, aes(x = state)) +
        geom_col(aes(y = other_count, fill = "Other Shapes"), position = "dodge") +
        geom_col(aes(y = selected_count, fill = input$shape_select), position = "dodge") +
        labs(x = "State", y = "Count", fill = "Shape") +
        theme_minimal()
    } else {
      p <- ggplot(filtered_data, aes(x = state, fill = shape)) +
        geom_bar() +
        labs(x = "State", y = "Count") +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = "text", hoverinfo="text", hovertext=~state)
  })
}

ui <- fluidPage(
  shape_ui("1")
)

server <- function(input, output, session) {
  callModule(shape_server, "1")
}
shinyApp(ui = ui, server = server)
