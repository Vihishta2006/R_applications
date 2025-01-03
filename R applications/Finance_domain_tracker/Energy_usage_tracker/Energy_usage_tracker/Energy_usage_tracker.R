library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Energy Usage Tracker"),
  sidebarLayout(
    sidebarPanel(
      numericInput("energy_usage", "Enter Energy Usage (kWh):", value = 0, min = 0),
      actionButton("log_energy", "Log Usage")
    ),
    mainPanel(
      h3("Energy Usage Graphs"),
      plotOutput("energy_bar_chart"),
      plotOutput("energy_scatter_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to store energy data
  energy_data <- reactiveVal(data.frame(day = numeric(), usage = numeric()))
  
  # Log energy usage
  observeEvent(input$log_energy, {
    new_entry <- data.frame(day = nrow(energy_data()) + 1, usage = input$energy_usage)
    energy_data(rbind(energy_data(), new_entry))
  })
  
  # Render bar chart
  output$energy_bar_chart <- renderPlot({
    ggplot(energy_data(), aes(x = factor(day), y = usage)) +
      geom_bar(stat = "identity", fill = "green") +
      labs(title = "Daily Energy Usage", x = "Day", y = "Energy (kWh)") +
      theme_minimal()
  })
  
  # Render scatter plot
  output$energy_scatter_plot <- renderPlot({
    ggplot(energy_data(), aes(x = day, y = usage)) +
      geom_point(color = "darkgreen", size = 3) +
      geom_line(color = "lightgreen", linetype = "dashed") +
      labs(title = "Energy Usage Trend", x = "Day", y = "Energy (kWh)") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
