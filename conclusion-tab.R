library(shiny)

conclusion_ui <- function() { 
    fluidPage(
      h1("Conclusions", style = "color: black; text-align: center;"),
      mainPanel(
      h3("General Conclusions:"),
      tags$ul(
        tags$li("Alien sightings increased considerably after 1994, likely due to increased aircraft traffic and also the emergence of the internet - many sightings prior were likely forgotten"),
        tags$li("The majority of sightings last less than a few minutes, likely due to many being astronomical or aeronautical objects."),
        tags$li("The majority of sightings take place at night, morning, and less so midday. At night it is much easier to see dully lit objects in the sky. Morning being more common than day is likely due to human activity such as commuting and airlines having most flights in the morning hours."),
        tags$li("The majority of sightings reported appear in urban cities, or areas with higher populations.")
      )
    )
  )}

server <- function(input, output, session) {
  # just leaving blank as there is no server logic required
}

shinyApp(ui = ui, server = server)
