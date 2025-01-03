# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Simulated data: Income and Housing Cost
set.seed(123)
data <- data.frame(
  Region = rep(c("Urban", "Suburban", "Rural"), each = 50),
  Income = c(rnorm(50, 50000, 10000), rnorm(50, 60000, 8000), rnorm(50, 40000, 7000)),
  HousingCost = c(rnorm(50, 1500, 300), rnorm(50, 1200, 200), rnorm(50, 800, 150))
)

# UI
ui <- fluidPage(
  titlePanel("Housing Affordability Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:", choices = unique(data$Region), selected = "Urban"),
      sliderInput("income_range", "Income Range:", 
                  min = min(data$Income), max = max(data$Income), 
                  value = c(min(data$Income), max(data$Income)))
    ),
    mainPanel(
      plotlyOutput("scatterPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter data based on selected region and income range
  filtered_data <- reactive({
    data %>%
      filter(Region == input$region) %>%
      filter(Income >= input$income_range[1], Income <= input$income_range[2])
  })
  
  # Scatter Plot: Income vs Housing Cost
  output$scatterPlot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Income, y = HousingCost)) +
      geom_point(aes(color = HousingCost), size = 3, alpha = 0.7) +
      labs(title = paste("Income vs Housing Costs in", input$region),
           x = "Income (USD)", y = "Housing Cost (USD)") +
      theme_minimal() +
      scale_color_gradient(low = "blue", high = "red")
    ggplotly(gg)
  })
}

# Run the app
shinyApp(ui, server)
