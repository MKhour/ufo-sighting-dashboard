library(shiny)
library(ggplot2)
library(shinythemes)
source('ufo-description-words.R')
source('ufo_sighting_bystate.R')
source('introduction-tab.R')
source('conclusion-tab.R')
source('ufo-sighting-by-year-and-shape.R')


# Define UI for application
ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage(
    id = "main_navbar",
    title = "UFO Sighting Dashboard",
    tabPanel("Introduction", value = "intro_tab", intro_ui()),
    tabPanel("Sighting Locations", value = "loc_tab", state_ui("4")),
    tabPanel("UFO Types & Sightings by Time", value = "time_tab", sightings_by_time_ui("1")),
    tabPanel("Sightings Descriptions", value = "word_tab", word_ui("3")),
    tabPanel("Conclusion", value = "conclusion_tab", conclusion_ui())
    )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$main_navbar, {
    if (input$main_navbar == "intro_tab") {
      
    }
    else if (input$main_navbar == "loc_tab") {
      callModule(state_server, "4")
    }
    else if (input$main_navbar == "time_tab") {
      callModule(sightings_by_time_server, "1")
    }
    else if (input$main_navbar == "type_tab") {
      callModule(shape_server, "2")
    }
    else if (input$main_navbar == "word_tab") {
      callModule(word_server, "3")
    }
    else if (input$main_navbar == "conclusion_tab") {
      
    }
  }, ignoreNULL = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
