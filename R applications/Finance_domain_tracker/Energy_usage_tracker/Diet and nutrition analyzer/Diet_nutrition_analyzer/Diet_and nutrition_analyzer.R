# Load required libraries
library(shiny)
library(ggplot2)
library(plotly)

# Recommended macronutrient distribution
recommended_data <- data.frame(
  Nutrient = c("Carbohydrates", "Proteins", "Fats"),
  Percentage = c(50, 20, 30)
)

# UI
ui <- fluidPage(
  titlePanel("Diet and Nutrition Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("carbs", "Carbohydrates (%)", min = 0, max = 100, value = 50),
      sliderInput("proteins", "Proteins (%)", min = 0, max = 100, value = 20),
      sliderInput("fats", "Fats (%)", min = 0, max = 100, value = 30),
      actionButton("update", "Update Custom Diet")
    ),
    
    mainPanel(
      plotlyOutput("pieChart"),
      textOutput("validationMessage")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store custom macronutrient distribution
  custom_data <- reactiveValues(
    Nutrient = recommended_data$Nutrient,
    Percentage = recommended_data$Percentage
  )
  
  # Update custom macronutrient distribution when "Update" button is clicked
  observeEvent(input$update, {
    total <- input$carbs + input$proteins + input$fats
    if (total == 100) {
      custom_data$Percentage <- c(input$carbs, input$proteins, input$fats)
      output$validationMessage <- renderText("")
    } else {
      output$validationMessage <- renderText("Error: Total percentage must equal 100%!")
    }
  })
  
  # Render pie chart for macronutrient distribution
  output$pieChart <- renderPlotly({
    pie_data <- data.frame(
      Nutrient = custom_data$Nutrient,
      Percentage = custom_data$Percentage
    )
    pie <- ggplot(pie_data, aes(x = "", y = Percentage, fill = Nutrient)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Macronutrient Distribution", fill = "Nutrient") +
      theme_void()
    ggplotly(pie)
  })
}

# Run the app
shinyApp(ui, server)