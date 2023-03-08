# ===============================================
# Fill in the following fields
# ===============================================
# Title: Stat 133: Project 2
# Description: Savings Rate Calculator
# Author: Isabel Adelhardt
# Date: 4/8/2022

# ===============================================
# Required packages
# ===============================================
library(shiny)
library(tidyverse)
# etc

# ===============================================
# Define User-Interface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Savings Simulatior"),
  
  fluidRow(
    # Input(s) for annual-income
    column(3,
           numericInput(inputId = "annual_income",
                        label = "Input your annual income",
                        value = 50000),
           sliderInput(inputId = "savings",
                       label = "What proportion of you annual income would you like to save",
                       min = 0.0, 
                       max = 1.0,
                       step = 0.05,
                       value = .05)
           ),
    
    # Input(s) for target-amount
    column(3,
           numericInput(inputId = "target_amount",label = "target amount", value = 1000000)
          ),
    
    # Input(s) for current-age
    column(3,
           numericInput(inputId = "age",
                        label = "current age",
                        value = 25),
           sliderInput(inputId = "age_achieve",
                       label = "age when you reach your target amount",
                       min = 50,
                       max = 80,
                       value = 65)
    ),
    
    # Input(s) for rate-of-return
    column(3,
           sliderInput(inputId = "rate_return",
                       label = "specify rate of return",
                       min = 0.0,
                       max = 1.0,
                       value = .05)
           )
  ),
  
  hr(),
  h4('Visulizing the relationship between the time it takes to reach target amount and the proportion of income you save each year'),
  plotOutput('plot1'),

  hr(),
  h4('Visualizing the relationship between the amount of money you contribute under a certain savings rate and the amount of money it turns into over time'),
  plotOutput('plot2'),
  
  hr(),
  h4('How different savings rate change the outcome of your savings'),
  DT::dataTableOutput('table')
)



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  dat <- reactive({
    savings_rate = seq(from = .05, to = 1.0, by = .05)
    annual_comp = round((input$annual_income * savings_rate), digits = 2)
    years = round((log10((input$rate_return * input$target_amount / annual_comp) + 1) / log10(1 + input$rate_return)), digits = 0)
    age_at_goal = round((years + input$age), digits = 0)
    goal_reached = age_at_goal <= input$age_achieve 
    total_comp = round((annual_comp * years), digits = 2)
    total_growth = round((input$target_amount - total_comp), digits = -2)
    percent_contrib = round((total_comp / input$target_amount * 100), digits = 2)
    percent_growth = 100 - percent_contrib
    savings_rate = savings_rate * 100
    data.frame(savings_rate, annual_comp, total_comp, total_growth, percent_contrib, percent_growth, years, age_at_goal, goal_reached)
  })
  
  
  # code for plot-1
  # (e.g. uses reactive data frame for graphing purposes)
  output$plot1 <- renderPlot({
    ggplot(data = dat(), aes(x = savings_rate, y = years, fill = goal_reached)) + 
      geom_bar(stat = "identity") +
      ggtitle("Years to Reach Target Amount with different Savings Rate") + 
      xlab("savings rate (%)") + 
      ylab("years to reach target amount") + theme_classic()
  })
  

  # code for plot-2
  # (e.g. uses reactive data frame for graphing purposes)
  output$plot2 <- renderPlot({
    ggplot(data = dat(), aes(x = savings_rate)) + 
      geom_point(aes(y = total_comp), color = "#FD3A2D", size = 5, fill = "#FFB092", alpha = 0.7, shape = 21, stroke = 2) + 
      geom_segment(aes(x = savings_rate, xend = savings_rate, y = 0, yend = total_comp), size = 1, color = "red", linetype = "longdash") +
      geom_point(aes(y = total_growth), color = "#1E2F97", size = 5, fill = "#4ADEDE", alpha = 0.7, shape = 21, stroke = 2) + 
      geom_segment(aes(x = savings_rate, xend = savings_rate, y = 0, yend = total_growth), size = 1, color = "blue", linetype = "dotdash") +
      ggtitle("Comparing Total Growth (red) and Total Contribution (blue) Under Various Savings Rates") + 
      theme_light() + 
      labs(x = 'savings rate (%)', y = 'dollars ($)')
  })

    
  # code for statistics
  output$table <- DT::renderDataTable({
    data_for_part2 = select(dat(), savings_rate, annual_comp, total_comp, total_growth, percent_contrib, percent_growth, years, age_at_goal)
    colnames(data_for_part2) = c('Savings Rate (%)', 'Annual Contribribution', 'Total Contribution', 'Total Growth', 'Percent Contribution', 'Percent Growth', 'Number of Years', 'Age at Target')
    data_for_part2
    })

  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

