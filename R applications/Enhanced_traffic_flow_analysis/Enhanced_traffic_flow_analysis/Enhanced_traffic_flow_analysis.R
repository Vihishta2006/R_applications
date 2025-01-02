library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Enhanced Traffic Flow Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("hour", "Hour of Day:", min = 0, max = 23, value = 12, step = 1),
      numericInput("vehicle_count", "Number of Vehicles:", value = 0, min = 0),
      actionButton("log_traffic", "Log Traffic Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotOutput("traffic_bar_chart")),
        tabPanel("Line Graph", plotOutput("traffic_line_chart")),
        tabPanel("Scatter Plot", plotOutput("traffic_scatter_plot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  traffic_data <- reactiveVal(data.frame(Hour = numeric(), Vehicles = numeric()))
  
  observeEvent(input$log_traffic, {
    new_entry <- data.frame(Hour = input$hour, Vehicles = input$vehicle_count)
    traffic_data(rbind(traffic_data(), new_entry))
  })
  
  # Bar chart
  output$traffic_bar_chart <- renderPlot({
    ggplot(traffic_data(), aes(x = factor(Hour), y = Vehicles)) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(title = "Traffic Flow - Bar Chart", x = "Hour of Day", y = "Number of Vehicles") +
      theme_light()
  })
  
  # Line graph
  output$traffic_line_chart <- renderPlot({
    ggplot(traffic_data(), aes(x = Hour, y = Vehicles)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(color = "red", size = 3) +
      labs(title = "Traffic Flow - Line Graph", x = "Hour of Day", y = "Number of Vehicles") +
      theme_minimal()
  })
  
  # Scatter plot
  output$traffic_scatter_plot <- renderPlot({
    ggplot(traffic_data(), aes(x = Hour, y = Vehicles)) +
      geom_point(color = "green", size = 3, alpha = 0.7) +
      labs(title = "Traffic Flow - Scatter Plot", x = "Hour of Day", y = "Number of Vehicles") +
      theme_light()
  })
}

# Run the app
shinyApp(ui, server)
