library(shiny)

ui <- navbarPage(
  "My Shiny App",
  tabPanel(
    "Conclusion",
    mainPanel(
      h1("Conclusions", style = "color: green; text-align: center;"),
      p("what sorts of conclusions"),
      h3("bullet points:"),
      tags$ul(
        tags$li("something"),
        tags$li("something"),
        tags$li(strong("something"))
      ),
      h4("extra info"),
      p("something here"),
      img(src = "ufo.jpg", height = 200, style = "display: block; margin: 0 auto;")
    )
  )
)

server <- function(input, output, session) {
  # just leaving blank as there is no server logic required
}

shinyApp(ui = ui, server = server)