library(shiny)
library(leaflet)
library(dplyr)
library(DT)

# Set the working directory
#setwd("~/Documents/DS_2003/Final Project/archive (2)")

# Load UFO sighting data
#ufo_data <- read.csv("ufo_scrubbed.csv")
#setwd('C:/Users/Kyle Tran/Downloads')
# ufo_data = read.csv("ufoData.csv")

# Remove states with empty labels
# ufo_data <- ufo_data[!ufo_data$state %in% c("", " "), ]

# Convert latitude and longitude to numeric
# ufo_data$latitude <- as.numeric(as.character(ufo_data$latitude))
# ufo_data$longitude <- as.numeric(as.character(ufo_data$longitude))

# Get unique states and their counts
#state_counts <- ufo_data %>%
#  group_by(state) %>%
#  summarise(count = n()) %>%
#  arrange(desc(count))

# Get top 5 states with the most UFO sightings
#top_states <- head(state_counts, 5)

# Sort unique states alphabetically for dropdown menu
unique_states <- sort(unique(ufo_data$state))

city_counts <- ufo_data %>%
  group_by(city) %>%
  summarize(count = n())

# Define UI
state_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  titlePanel("UFO Sightings in the United States"),
  navbarPage("",
    tabPanel("View Map and States",
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
    ),
    tabPanel("Compare Cities",
       sidebarLayout(
         sidebarPanel(
           selectInput(ns('city'), "View Cities By:", choices = c("Descending Order", "Ascending Order", Search = "Search")),
           h4("Cities and the Number of UFO Sightings"),
           numericInput(ns("page"), "Page Number:", value = 1, min = 1, max = nrow(city_counts)),
           uiOutput(ns("searchUI")),

           dataTableOutput(ns("current_cities_table")),
         ),
         mainPanel(
           plotOutput(ns("city_bar_chart")),
           p("This is a bar graph with the total number of UFO spottings per city")
         )
       )
    )
  )
)}

# Define server logic
state_server <- function(input, output, session) {
  ns <- session$ns
  
  output$searchUI <- renderUI({
    if (input$city == "Search") {
      textInput(ns("search"), "Search City:", "")
    } else {
      NULL
    }
  })
  
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
  
  # ==== City logic ==============================================================
  
  
  filtered_city_data <- reactive({
      data <- subset(city_counts, grepl(tolower(input$search), tolower(city)))
      if(nrow(data) > 0) {
      data$order <- c(1:nrow(data))
      return(data)
      }
      else return(NULL)
  })
  
  sorted_city_counts <- reactive({
    if(input$city == 'Descending Order') {
      arranged <- city_counts %>%
            arrange(desc(count))
      arranged$order <- c(1:nrow(arranged))
      rownames(arranged) <- c(1:nrow(arranged))
      return(arranged)
      
    }
    else if(input$city == 'Ascending Order') {
      arranged <- city_counts %>%
        arrange(count)
      arranged$order <- c(1:nrow(arranged))
      rownames(arranged) <- c(1:nrow(arranged))
      return(arranged)
    }
    else {
      data <- filtered_city_data()
      if(is.null(data)) {
        return(NULL)
      }
      
      data$order <- c(1:nrow(data))
      
      return(data)
    }
  })
  
  output$current_cities_table <- renderDataTable({
    datatable(sorted_city_counts()[((input$page - 1) * 10 + 1):(input$page * 10),], 
      options = list(
        info = FALSE,
        rownames = 'order',
        searching = FALSE,
        paging = FALSE,
        columnDefs = list(list(className = 'dt-wrap', targets = "_all"))
      )
    )
    
  })
  
  
  output$city_bar_chart <- renderPlot({
    
    if(!is.null(sorted_city_counts())) {
      sub_data <- sorted_city_counts()[((input$page - 1) * 10 + 1):(input$page * 10),]
      
    ggplot(sub_data, aes(x = reorder(city, order), y = count, fill = city)) +
      geom_bar(stat = "identity") +
      scale_fill_discrete(breaks = sub_data$order) +
      labs(title = "UFO Spottings by City", x = "City", y = "Count") +
      theme_minimal() + theme(text = element_text(size = 16))
    }
    
  })
}

ui <- fluidPage(
  state_ui("1")
)

server <- function(input, output, session) {
  callModule(state_server, "1")
}

# Run the application
shinyApp(ui = ui, server = server)


