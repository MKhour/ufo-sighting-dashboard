library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(stringr)
library(tidytext) # for unnest_tokens function
library(wordcloud2)

#setwd('~/Downloads/archive')

# Load UFO sighting data
#ufo_data <- read.csv("scrubbed.csv")
#setwd('C:/Users/Kyle Tran/Downloads')
#ufo_data = read.csv("ufoData.csv")

#regex <- "&#\\d{1,}"

#ufo_data_clean_comments <- ufo_data %>% 
#  mutate(
#    comments = lapply(comments, 
#                      function(og_comment) {
#                        gsub(regex, "", og_comment)
#                      }),
#    year = as.numeric(format(as.Date(datetime, format="%m/%d/%Y %H:%M"),"%Y"))
#  )

# Define UI for application
word_ui <- function(id) { 
  ns <- NS(id)
  fluidPage(
  titlePanel("Descriptions of UFO Sightings"),
  hr(),
  h3("Word Cloud"),
  p("The word cloud below shows words that commonly appear in descriptions of UFO sightings."),
  p("Note that due to the small number of reported UFOs from before 1960, the minimum occurrence threshold may need to be decreased for any words to be displayed in the plot."),
  sidebarLayout(
    sidebarPanel(
      sliderInput(ns("threshold"),
                  "Minimum occurence of word:", min = 0, max = 10000, value = 1000, step = 100),
      sliderInput(ns("year_range"),
                    "Year range:", min = 1910, max = 2014, value = c(1960, 2014)),
               ),
      mainPanel(
        (wordcloud2Output(ns('word_cloud')))
      )
  ),
  hr(),
  h3("Description Search"),
  p("All UFO sighting descriptions are displayed in the table below."),
  p("You may search for key words via the bar to the right."),
  DT::dataTableOutput(ns('comment_table'))
)}


# Define server logic
word_server <- function(input, output, session) {
  
  word_counts <- reactive({
    req(input$year_range)
    req(input$threshold)
    
    filtered_years <- ufo_data_clean_comments %>%
      filter(Year >= as.numeric(input$year_range[1]) & Year <= as.numeric(input$year_range[2]))
    
    all_words <- unnest_tokens(filtered_years, "word", Description) 
    
    word_count_table <- all_words %>%
      # remove common prepositions and articles
      filter(!(word %in% c("in", "and", "of", "from", "the", "to", "i", "it", "a", "an", "on", "at", "by", "like", "quot", "was", "is", "we"))) %>%
      # remove pure numbers
      filter(!grepl("\\d{1,}", word)) %>% 
      # calculate word counts
      count(word, name = "count") %>%
      # filter based on word count
      filter(count >= input$threshold)
    
    return(word_count_table)
  })
  
  output$word_cloud <- renderWordcloud2({
    wordcloud2(data = word_counts())
  })
  
  output$comment_table <- DT::renderDataTable(DT::datatable({
    ufo_data_clean_comments %>%
      arrange(Year) %>%
      select(Date, State, Country, Description)
  }), rownames = FALSE)
  
}

ui <- fluidPage(
  word_ui("1")
)

server <- function(input, output, session) {
  callModule(word_server, "1")
}
# Run the application 
shinyApp(ui = ui, server = server)


