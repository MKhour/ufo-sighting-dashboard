library(shiny)
library(ggplot2)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage(
    title = "UFO Sighting Dashboard",
    tabPanel("Sighting Locations",
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 plotOutput("graph")
               )
             )
    ),
    tabPanel("Sightings by Time"),
    tabPanel("UFO Types"),
    tabPanel("Sightings Descriptions")
    )
)

# Define server logic
server <- function(input, output) {
  output$graph <- renderPlot({
    ggplot()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
