# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Simulated data: Income and Food Expense
set.seed(456)
food_data <- data.frame(
  Lifestyle = rep(c("Minimalist", "Moderate", "Luxury"), each = 50),
  Income = c(rnorm(50, 30000, 5000), rnorm(50, 50000, 8000), rnorm(50, 80000, 12000)),
  FoodExpense = c(rnorm(50, 300, 50), rnorm(50, 500, 70), rnorm(50, 800, 100))
)

# UI
ui <- fluidPage(
  titlePanel("Food Expense Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectInput("lifestyle", "Select Lifestyle:", choices = unique(food_data$Lifestyle), selected = "Minimalist"),
      sliderInput("income_range", "Income Range:", 
                  min = min(food_data$Income), max = max(food_data$Income), 
                  value = c(min(food_data$Income), max(food_data$Income)))
    ),
    mainPanel(
      plotlyOutput("scatterPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter data based on selected lifestyle and income range
  filtered_data <- reactive({
    food_data %>%
      filter(Lifestyle == input$lifestyle) %>%
      filter(Income >= input$income_range[1], Income <= input$income_range[2])
  })
  
  # Scatter Plot: Income vs Food Expense
  output$scatterPlot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Income, y = FoodExpense)) +
      geom_point(aes(color = FoodExpense), size = 3, alpha = 0.7) +
      labs(title = paste("Income vs Food Expenses for", input$lifestyle, "Lifestyle"),
           x = "Income (USD)", y = "Food Expense (USD)") +
      theme_minimal() +
      scale_color_gradient(low = "green", high = "orange")
    ggplotly(gg)
  })
}

# Run the app
shinyApp(ui, server)
