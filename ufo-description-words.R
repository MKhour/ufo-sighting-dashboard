library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(stringr)
library(tidytext) # for unnest_tokens function
library(wordcloud2)

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
                  "Minimum occurence of word:", min = 0, max = 10000, value = 2000, step = 100),
      sliderInput(ns("year_range"),
                    "Year range:", min = 1910, max = 2014, value = c(1960, 2014)),
               ),
      mainPanel(
        (wordcloud2Output(ns('word_cloud')))
      )
  ),
  hr(),
  h3("Sentiment Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(ns("sentiment_year_range"),
                  "Year range:", min = 1910, max = 2014, value = c(1960, 2014)),
    ),
    mainPanel(
      plotlyOutput(ns("sentiment_bar_graph"))
    )
  ),
  hr(),
  h3("Description Search"),
  p("All UFO sighting descriptions are displayed below. Search for key words via the bar to the right."),
  DT::dataTableOutput(ns('comment_table'))
)}


# Define server logic
word_server <- function(input, output, session) {
  
  word_counts <- reactive({
    req(input$year_range)
    req(input$threshold)
    
    filtered_years <- ufo_with_sentiment_data %>%
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
  
  sentiment_counts <- reactive({
    req(input$sentiment_year_range)
    
    filtered_years <- ufo_with_sentiment_data %>%
      filter(Year >= as.numeric(input$sentiment_year_range[1]) & Year <= as.numeric(input$sentiment_year_range[2])) %>%
      select(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive)
    
    df <- data.frame(colSums(filtered_years))
    sentiment_count_table <- df %>%
      mutate(
        sentiment = rownames(df),
        count = as.integer(colSums.filtered_years.)
      ) %>% select(sentiment, count)
    
    return(sentiment_count_table)
  })
  
  output$old_sentiment_bar_graph <- renderPlot({
    
    emotions_plot <- ggplot(data=sentiment_counts(), aes(x=sentiment, y=count)) +
      geom_bar(stat="identity") +
      coord_flip()
    
    return(emotions_plot)
  })
  
  output$sentiment_bar_graph <- renderPlotly({
    
    p <- plot_ly(data = sentiment_counts(), x=~count, y=~sentiment, type='bar', color = ~count,
                 hoverinfo = "x") %>%
      layout(title ="Occurrence of Sentiments",
             xaxis = list(title="Count"),
             yaxis = list(title="Sentiment"))
    return(p)
  })
  
  output$comment_table <- DT::renderDataTable(DT::datatable({
    ufo_with_sentiment_data %>%
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


