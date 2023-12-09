library(shiny)
library(ggplot2)
library(readxl)

# Read data
Amazon_Sale_Report <- read_excel("~/Greg's School/ISTA 498/Amazon Sale Report.xlsx")
ASR <- Amazon_Sale_Report

# Define UI
ui <- fluidPage(
  titlePanel("Sales Prediction with Logistic Regression"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Features"),
      selectInput("feature1", "Select 1st Feature", choices = colnames(ASR)),
      selectInput("feature2", "Select 2nd Feature", choices = colnames(ASR)),
      selectInput("promotionId", "Select Promotion ID", choices = unique(ASR$promotion_id)),
      actionButton("trainButton", "Train Model"),
      br(),
      h5("Model Accuracy:"),
      textOutput("accuracyText")
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to train the logistic regression model
  trained_model <- eventReactive(input$trainButton, {
    if (input$trainButton > 0) {
      formula <- as.formula(paste("promotion_id_binary ~", input$feature1, "+", input$feature2))
      log_model <- glm(formula, family = "binomial", data = ASR)
      return(log_model)
    }
  })
  
  # Reactive expression for model accuracy
  accuracy <- eventReactive(input$trainButton, {
    if (!is.null(trained_model())) {
      predictions <- predict(trained_model(), newdata = ASR, type = "response")
      promotion_id_binary <- ifelse(ASR$promotion_id == input$promotionId, 1, 0)
      accuracy_score <- mean(round(predictions) == promotion_id_binary)
      return(accuracy_score)
    }
  })
  
  # Output to display model accuracy
  output$accuracyText <- renderText({
    if (!is.null(accuracy())) {
      paste("Model Accuracy:", round(accuracy() * 100, 2), "%")
    }
  })
  
  # Output to display the scatterplot
  output$scatterplot <- renderPlot({
    if (!is.null(trained_model())) {
      predictions <- predict(trained_model(), newdata = ASR, type = "response")
      
      ggplot(data.frame(Actual = ifelse(ASR$promotion_id == input$promotionId, 1, 0), Predicted = predictions), aes(x = Actual, y = Predicted)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        labs(title = paste("Actual vs. Predicted for Promotion ID:", input$promotionId), x = "Actual Promotion", y = "Predicted Probability")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)