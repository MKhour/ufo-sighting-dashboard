library(shiny)

ui <- navbarPage(
  "My Shiny App",
  tabPanel(
    "Introduction",
    mainPanel(
      h1("UFO Sightings", style = "color: green; text-align: center;"),
      p("introduction header"),
      h3("bullet points:"),
      tags$ul(
        tags$li("insert"),
        tags$li("insert"),
        tags$li(strong("insert"))
      ),
      h4("insert data"),
      p("insert data"),
      img(src = "ufo2.jpg", height = 200, style = "display: block; margin: 0 auto;")
    )
  )
)

server <- function(input, output, session) {
  # just leaving blank as there is no server logic required
}

shinyApp(ui = ui, server = server)