library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Water Consumption Monitor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("daily_usage", "Daily Water Usage (liters):", value = 0),
      actionButton("log_usage", "Log Usage")
    ),
    mainPanel(
      plotOutput("daily_usage_chart"),
      plotOutput("scatterplot_chart")
    )
  )
)

# Server
server <- function(input, output, session) {
  water_data <- reactiveVal(data.frame(day = numeric(), usage = numeric()))
  
  observeEvent(input$log_usage, {
    new_entry <- data.frame(day = nrow(water_data()) + 1, usage = input$daily_usage)
    water_data(rbind(water_data(), new_entry))
  })
  
  # Bar chart for daily usage
  output$daily_usage_chart <- renderPlot({
    ggplot(water_data(), aes(x = factor(day), y = usage)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Daily Water Usage", x = "Day", y = "Liters")
  })
  
  # Scatter plot for daily water usage
  output$scatterplot_chart <- renderPlot({
    ggplot(water_data(), aes(x = day, y = usage)) +
      geom_point(color = "darkblue", size = 3) +
      geom_line(color = "lightblue") +
      labs(title = "Daily Water Usage Scatter Plot", x = "Day", y = "Liters")
  })
}

shinyApp(ui, server)
