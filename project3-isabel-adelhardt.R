# ===============================================
# Title: Stat 133 Project 3
# Description: : "State of the Union" Text Analysis
# Author: Isabel Adelhardt
# Date: 4/29/2022


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(ggraph)


# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")
dat <- read.csv(file = "state-union-2001-2022.csv", sep = ",", header = TRUE)

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Text Analysis of State of the Union Speeches from 2001 to 2022"),
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("How would you like to visualize the most frequent words used")),
           radioButtons(inputId = "choose", 
                        label = "Choose one option", 
                        choices = c("wordcloud", "barchart"), 
                        selected = "barchart")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Which president's speech(es) would you like to analyize")),
           selectInput(inputId = "pres", 
                       label = "Make a selection",
                       choices = c("George W. Bush",
                                   "Barack Obama",
                                   "Donald J. Trump",
                                   "Joseph R. Biden", 
                                   "all presidents" = "all"),
                       selected = "Joseph R. Biden"),
           checkboxInput(inputId = "sentiments",
                         label = "Would you like to see the associated sentiment of the most frequently used words",
                         value = FALSE)
    ),
    
    # replace with your widgets
    column(3,
           p(em("How would you like to analyze the visualiza")),
           radioButtons(inputId = "arrange", 
                        label = "Bars in order of:", 
                        choices = c("decreasing freq" = "arr_dec",
                                    "increasing freq" = "arr_inc"),
                        selected = "arr_dec")
    ),
    
    # replace with your widgets
    column(3,
           p(em("How many of the most frequently used words would you like to analyze")),
           sliderInput(inputId = "num_words",
                       label = "Pick a number of words",
                       min = 10,
                       max = 100,
                       value = 10)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Freq Words",
                       h3("Most Frequent Words Used After Removing Stop Words"),
                       plotOutput("barplot"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Sentiment Analysis", 
                       h3("Most Frequently Used Words contributing to the Positive and Negative Seniments"),
                       plotOutput("histogram"),
                       hr(),
                       verbatimTextOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  tokenized_nostop <- reactive({
    t = dat %>% unnest_tokens(output = word, input = message) %>% anti_join(stop_words, by = "word")
    if (input$pres != "all") {
      t = t %>% filter(president == input$pres)
    }
    t
  })
  word_occurences <- reactive({
    counting = tokenized_nostop() %>% count(president, word, sort = TRUE) %>% ungroup()
    counting
  })
  
  sent_of_pres <- reactive({
    sent = word_occurences() %>% inner_join(sentiments, by = "word") 
    sent
  })
  
  most_neg_pos <- reactive({
    bing_word_counts = tokenized_nostop() %>% inner_join(sentiments, by = "word") %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup() 
    bing_word_counts
  })
  
  sentiment_dat_wrdcld <- reactive({
    if (input$pres != "all"){
      wordcloud_dat = tokenized_nostop() %>% 
        filter(president == input$pres) %>%
        anti_join(stop_words, by = "word") %>%
        inner_join(sentiments, by = "word") %>%
        count(word, sentiment, sort = TRUE) %>%
        reshape2::acast(word ~ sentiment, value.var = "n", fill = 0)
    } else {
      wordcloud_dat =  tokenized_nostop() %>% 
        anti_join(stop_words, by = "word") %>%
        inner_join(sentiments, by = "word") %>%
        count(word, sentiment, sort = TRUE) %>%
        reshape2::acast(word ~ sentiment, value.var = "n", fill = 0)
    }
    wordcloud_dat
  })
  
  # ===============================================
  # Outputs for the first TAB (i.e. barchart)
  # ===============================================
  
  # code for barplot
  output$barplot <- renderPlot({
    dat_for_plot = sent_of_pres() %>% slice_head(n = input$num_words)
    if (input$choose == "barchart") {
      if (input$sentiments == TRUE) {
        if (input$arrange == "arr_dec"){
          plot = ggplot(data = dat_for_plot) + 
            geom_col(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
            scale_fill_manual(values = c("#F2A5E8", "#F28500")) + xlab(NULL) +
            coord_flip() +
            labs(title = "Most Frequent Words with Associated Sentiment") 
        } else {
          plot = ggplot(data = dat_for_plot) + 
            geom_col(aes(x = reorder(word, -n), y = n, fill = sentiment)) + 
            scale_fill_manual(values = c("#F2A5E8", "#F28500")) + xlab(NULL) +
            coord_flip() +
            labs(title = "Most Frequent Words with Associated Sentiment") 
        }
      } else {
        if (input$arrange == "arr_dec"){
          plot = ggplot(data = dat_for_plot) + 
            geom_col(aes(x = reorder(word, n), y = n)) +
            xlab(NULL) +
            coord_flip() +
            labs(title = "Most Frequent Words")
        } else {
          plot = ggplot(data = dat_for_plot) + 
            geom_col(aes(x = reorder(word, -n), y = n)) +
            xlab(NULL) +
            coord_flip() +
            labs(title = "Most Frequent Words")
        }
      }
    } else {
      tidied = tokenized_nostop() %>% count(word)
      plot = wordcloud(
        words = tidied$word,
        freq = tidy_union$n,
        max.words = input$num_words,
        random.order = FALSE,
        colors = brewer.pal(8, "Dark2"))
    }
    plot + theme_minimal()
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    if (input$sentiments) {
      sent_of_pres() %>% select(word, n, sentiment)
    } else {
      word_occurences() %>% select(word, n)
    }
    
  })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. histogram)
  # ===============================================
  
  # code for histogram
  output$histogram <- renderPlot({
    if (input$choose == "wordcloud") {
      plot = comparison.cloud(
        sentiment_dat_wrdcld(),
        scale = c(4, 0.1),
        colors = c("#F2A5E8", "#F28500"),
        max.words = input$num_words)
    } else {
      data_sent = most_neg_pos() %>% group_by(sentiment) %>% ungroup() %>% slice_head(n = 2* input$num_words)
      if (input$arrange == "arr_dec") {
        plot = ggplot(data_sent, aes(x = reorder(word, n) , n, fill = sentiment)) +
          geom_col(show.legend = FALSE) + scale_fill_manual(values = c("#F2A5E8", "#F28500")) + 
          facet_wrap(~sentiment, scales = "free_y") + 
          labs(y = "Contribution to sentiment", 
               x = NULL,
               title = "Which words contributed to the sentiment of the speech") + 
          coord_flip()
      } else {
        plot = ggplot(data_sent, aes(x = reorder(word, -n) , y = n, fill = sentiment)) +
          geom_col(show.legend = FALSsE) + scale_fill_manual(values = c("#F2A5E8", "#F28500")) + 
          facet_wrap(~sentiment, scales = "free_y") + 
          labs(y = "Contribution to sentiment", 
               x = NULL,
               title = "Which words contributed to the sentiment of the speech") + 
          coord_flip()
      }
    }
    plot
  })
  
  # code for statistics
  output$table2 <- renderPrint({
    tbl2 = most_neg_pos() %>% slice_head(n = 2* input$num_words)
    tbl2
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

