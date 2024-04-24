library(shiny)

intro_ui <- function() {
  fluidPage(
    h1("Introduction", style="text-align: center; color: black;"),
    p("For our final project, we chose to do an analysis on UFO sightings. We gathered our data from Kaggle, with the 
        data source from the National UFO Reporting Center.", style="text-align: center;"),
    mainPanel(
      h3("The Dataset"),
      tags$ul(
        tags$li("Dataset: https://www.kaggle.com/datasets/NUFORC/ufo-sightings"),
        tags$li("Utilized the cleaned dataset which had no missing sighting times and was roughly 8% smaller than the raw dataset"),
        tags$li("Includes 70,000 sightings ranging from 1910 to 2014")
      ),
      h3("Our 3 Questions:"),
      tags$ul(
        tags$li("In what areas are UFO sightings most common?"),
        tags$li("How did the number of sightings change over time? In addition, what is the correlation between the duration of a sighting and its shape?"),
        tags$li("What words/topics come up frequently in descriptions of UFO sightings?")
      )
    )
)}


server <- function(input, output, session) {
  # just leaving blank as there is no server logic required
}

shinyApp(ui = ui, server = server)