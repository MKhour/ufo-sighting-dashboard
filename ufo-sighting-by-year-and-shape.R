library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(ggplot2)

#Getting cleaned data
#ufo_data <- readRDS(file="results.Rda")
ufo_data <- read.csv("sentiments_and_comments.csv")


# Get unique states and their counts
state_counts <- ufo_data %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Get top 5 states with the most UFO sightings
top_states <- head(state_counts, 5)

# Sort unique states alphabetically for dropdown menu
# unique_states <- sort(unique(ufo_data$state))

city_counts <- ufo_data %>%
  group_by(city) %>%
  summarize(count = n())

###################################################################################################

#2. Creating UI for Page
sightings_by_time_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  titlePanel("UFO Shapes & Sightings Over Time"),
  p("On this page, we focused on analyzing the frequency of UFO sightings based on reported UFO shapes and time of day. We also 
  looked into the relationship between shape of UFOs and duration of sighting. We also considered variables like year and state in our analysis.
  We provided our visualizations throught 3 different mediums: an interactive map, a scatterplot, and a barchart."),
  sidebarLayout(
    sidebarPanel(
      sliderInput(ns("year_range"), "Select Year", min = min(ufo_data$year), max = max(ufo_data$year), value = c(min(ufo_data$year), max(ufo_data$year))),
      checkboxGroupInput(ns("time_of_day"), "Time Of Day", choices = c("Morning"="Morning", "Day"="Day", "Night"="Night"), selected=c("Morning", "Day", "Night")),
      selectInput(ns("states"), "State", choices = c("All Entries", sort(unique(ufo_data$state)))),
      selectInput(ns("shape_select"), "Select Shape", choices = c("All Shapes",unique(ufo_data$shape))),
      numericInput(ns("min_duration"), "Minimum Duration (seconds)", min = min(ufo_data$duration, na.rm = TRUE), value = min(ufo_data$duration, na.rm = TRUE)),
      numericInput(ns("max_duration"), "Maximum Duration (seconds)", min = min(ufo_data$duration, na.rm = TRUE), value = max(ufo_data$duration, na.rm = TRUE))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput(ns("ufo_map_by_time"))),
        tabPanel("Scatterplot", plotOutput(ns("scatter_plot"))),
        tabPanel("Barchart", plotlyOutput(ns("ufo_plot_bar_graph")))
      )
    )
  )
)
}

#3. Creating Server for Page
sightings_by_time_server <- function(input, output, session) {
  #a. filtering data
  filtered_data <- reactive({
    req(input$states, input$year_range, input$time_of_day, input$shape_select, input$min_duration, input$max_duration)
    data <- ufo_data %>% filter(year >= input$year_range[1], year <= input$year_range[2], time_of_day %in% input$time_of_day,
               !is.na(duration), duration >= input$min_duration, duration <= input$max_duration, 
               if (input$states != "All Entries") state == input$states else TRUE, 
               if (input$shape_select != "All Shapes") shape == input$shape_select else TRUE)
    return(data)
    })
  
  #b. graphing bar graph for time/shape
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
                popup = ~paste("Shape:", shape, "<br>", "Duration:", duration, "seconds", 
                               "<br>", "City:", city, "State:", state,"<br>",
                               "Timestamp:",datetime, "<br>", "Comment:", comments),
                clusterOptions = markerClusterOptions())
 })
  
 
 #d. plotting scatterplot
 output$scatter_plot <- renderPlot({
   ggplot(filtered_data(), aes(x = duration, y = shape, color = shape)) +
     geom_point() +
     theme_minimal() +
     ggtitle("Visualizing Shape Categories Across Duration of Sightings") +
     xlab("Duration (seconds)") +
     ylab("Shape Type")
 })
}


ui <- fluidPage(
  sightings_by_time_ui("1")
)

server <- function(input, output, session) {
  callModule(sightings_by_time_server, "1")
}

shinyApp(ui = ui, server = server)









