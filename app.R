library(shiny)
library(ggplot2)
library(shinythemes)
source('ufo_shape_graphs.R')
source("ufo-sighting-by-year.R")

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage(
    id = "main_navbar",
    title = "UFO Sighting Dashboard",
    tabPanel("Sighting Locations", value = "loc_tab",
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 plotOutput("graph")
               )
             )
    ),
    tabPanel("Sightings by Time", value = "time_tab", sightings_by_time_ui("1")),
    tabPanel("UFO Types", value = "type_tab", shape_ui("2")),
    tabPanel("Sightings Descriptions")
    )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$main_navbar, {
    if (input$main_navbar == "time_tab") {
      callModule(sightings_by_time_server, "1")
    }
    else if (input$main_navbar == "type_tab") {
      callModule(shape_server, "2")
    }
  }, ignoreNULL = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
