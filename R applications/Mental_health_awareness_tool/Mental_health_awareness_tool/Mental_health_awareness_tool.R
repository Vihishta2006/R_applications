library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Mental Health Awareness Tool"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("stress_level", "Your Stress Level (1-10):", min = 1, max = 10, value = 5),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("stress_histogram"),
      plotOutput("stress_correlation")
    )
  )
)

# Server
server <- function(input, output, session) {
  stress_data <- reactiveVal(data.frame(stress = numeric(), productivity = numeric()))
  
  observeEvent(input$submit, {
    new_entry <- data.frame(stress = input$stress_level, productivity = rnorm(1, mean = 7 - input$stress_level))
    stress_data(rbind(stress_data(), new_entry))
  })
  
  # Histogram of stress levels
  output$stress_histogram <- renderPlot({
    ggplot(stress_data(), aes(x = stress)) +
      geom_histogram(binwidth = 1, fill = "red", alpha = 0.7) +
      labs(title = "Stress Levels Histogram", x = "Stress Level", y = "Frequency")
  })
  
  # Scatter plot for stress vs productivity
  output$stress_correlation <- renderPlot({
    ggplot(stress_data(), aes(x = stress, y = productivity)) +
      geom_point(color = "blue") +
      labs(title = "Stress vs Productivity", x = "Stress Level", y = "Productivity")
  })
}

shinyApp(ui, server)
