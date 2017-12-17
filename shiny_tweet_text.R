library(shiny)
library(tidyverse)
library(knitr)
library(devtools)
library(twitteR)
library(wordcloud)
library(tidytext)
library(streamR)
library(ROAuth)
library(reshape)
library(reshape2)
library(dplyr)
library(ggplot2)
library(splitstackshape)
library(plotly)
library(grid) 
library(lubridate)

#get tokenized data from twitter
tidy_tweets <- read.csv("tidy.csv")
#cleaned text
tweets_text <- read.csv("text.csv")
#bing
bing <- read.csv("attitude.csv")

ui <- shinyUI(fluidPage(
  titlePanel("Canada Goose Twitter Analysis"),
  navbarPage(title = "Texts Analysis EDA",
             tabPanel("Word frequency", plotOutput("wordfreq")),
             tabPanel("Sentiments", plotOutput("senti")),
             tabPanel("Word Cloud", plotOutput("cloud")),
             tabPanel("Comparison Word Cloud", plotOutput("comp")),
             tabPanel("Attitude", plotOutput("attitude"))

  
    )#end navarpage
  )#end fluidpage
)#end ui

server <- shinyServer(function(input, output){
  output$wordfreq <- renderPlot({  
    # freq plot
    tidy_tweets %>%
      count(word, sort = TRUE) %>%
      filter(n > 100) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(colour="red") +
      xlab(NULL) +
      ylab("Count") +
      coord_flip()
  })#end 1st output
  
  output$senti <- renderPlot({
    
    tweets_text%>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Sentiment Categories",
           x = NULL) +
      coord_flip()
  })#end 2nd output
  
  output$cloud <- renderPlot({
    tidy_tweets %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word,
                     n, 
                     max.words = 100,
                     colors = brewer.pal(8,"Dark2")))
  })#end 3rd output
  
  output$comp <- renderPlot({
    tidy_tweets %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 100)
  })#end 4th output
  
  output$attitude <- renderPlot({
    bing%>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Positive and Negative words",
           x = NULL) +
      coord_flip()
  })#end 5th output
})#end server

shinyApp(ui = ui, server = server)