library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(stringr)
library(tidytext) # for unnest_tokens function
library(wordcloud2)

setwd('~/Downloads/archive')

# Load UFO sighting data
ufo_data <- read.csv("scrubbed.csv")

regex <- "&#\\d{1,}"

ufo_data_clean_comments <- ufo_data %>% 
  mutate(
    comments = lapply(comments, 
                      function(og_comment) {
                        gsub(regex, "", og_comment)
                      }),
    year = as.numeric(format(as.Date(datetime, format="%m/%d/%Y %H:%M"),"%Y"))
  )

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage(
    title = "UFO Sighting Dashboard",
    tabPanel("Sighting Locations"),
    tabPanel("Sightings by Time"),
    tabPanel("UFO Types"),
    tabPanel("Sighting Descriptions",
             titlePanel("Descriptions of UFO Sightings"),
             p("The word cloud below shows words that commonly appear in descriptions of UFO sightings."),
             p("Note that due to the small number of reported UFOs from before 1960, the minimum occurrence threshold may need to be decreased for any words to be displayed in the plot."),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("threshold",
                             "Minimum occurence of word:",
                             min = 0, max = 10000, value = 1000, step = 100),
                 sliderInput("year_range",
                             "Year range:",
                             min = 1910, max = 2014, value = c(1960, 2014)),
               ),
               mainPanel(
                 wordcloud2Output('word_cloud')
                 # textOutput("debug")
                 # tableOutput('comment_table')
               )
             )
    ),
  )
)

# Define server logic
server <- function(input, output) {
  
  word_counts <- reactive({
    req(input$year_range)
    req(input$threshold)
    
    filtered_years <- ufo_data_clean_comments %>%
      filter(year >= as.numeric(input$year_range[1]) & year <= as.numeric(input$year_range[2]))
    
    all_words <- unnest_tokens(filtered_years, "word", comments) 
    
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
  
  output$comment_table <- renderTable({
    return(head(ufo_data_clean_comments,10))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
