# Random Forest Model

library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(readxl)
library(tidymodels)
library(ranger)  # Use ranger instead of randomForest

# Load data
Amazon_Sale_Report <- read_excel("~/Greg's School/ISTA 498/Amazon Sale Report.xlsx")
ASR <- Amazon_Sale_Report

ui <- dashboardPage(
  dashboardHeader(title = "Random Forest Model", titleWidth = 300),
  dashboardSidebar(
    selectInput("variable1", "Variable 1", choices = colnames(ASR)),
    selectInput("variable2", "Variable 2", choices = colnames(ASR)),
    selectInput("variable3", "Variable 3", choices = colnames(ASR)),
    actionButton("update", "Update Model")
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Random Forest Tree",
        status = "primary",
        solidHeader = TRUE,
        plotOutput("treePlot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    ASR %>%
      select("Amount", input$variable1, input$variable2, input$variable3)
  })
  
  observeEvent(input$update, {
    
    # Check if there are enough rows in the data for modeling
    if (nrow(filtered_data()) < 5) {
      output$treePlot <- renderPlot(NULL)
      return(NULL)
    }
    
    # Change from randomForest to ranger for compatibility with randomForestExplainer
    model <- ranger::ranger(Amount ~ ., data = filtered_data())
    
    # Plot the first tree
    tree_plot <- randomForest::randomForest(model, main = "First Tree")
    
    output$treePlot <- renderPlot({
      plot(tree_plot$forest[[1]], main = "First Tree")
    })
  })
}

shinyApp(ui, server)
