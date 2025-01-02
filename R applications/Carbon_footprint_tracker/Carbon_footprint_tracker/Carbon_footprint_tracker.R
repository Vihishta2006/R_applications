library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Carbon Footprint Tracker"),
  sidebarLayout(
    sidebarPanel(
      numericInput("km_driven", "Kilometers Driven (car):", value = 0),
      numericInput("meals_meat", "Meals with Meat:", value = 0),
      numericInput("electricity_usage", "Electricity Usage (kWh):", value = 0),
      actionButton("calculate", "Calculate Footprint")
    ),
    mainPanel(
      h3("Your Daily Carbon Footprint (kg CO2):"),
      verbatimTextOutput("footprint_result"),
      plotOutput("footprint_graph")
    )
  )
)

# Server
server <- function(input, output, session) {
  observeEvent(input$calculate, {
    # Calculate individual contributions
    driving_footprint <- input$km_driven * 0.21
    meals_footprint <- input$meals_meat * 2.5
    electricity_footprint <- input$electricity_usage * 0.45
    
    # Calculate total footprint
    total_footprint <- driving_footprint + meals_footprint + electricity_footprint
    
    # Display total footprint
    output$footprint_result <- renderText(round(total_footprint, 2))
    
    # Prepare data for the graph
    footprint_data <- data.frame(
      Activity = c("Driving", "Meals", "Electricity"),
      Contribution = c(driving_footprint, meals_footprint, electricity_footprint)
    )
    
    # Render graph
    output$footprint_graph <- renderPlot({
      ggplot(footprint_data, aes(x = Activity, y = Contribution, fill = Activity)) +
        geom_bar(stat = "identity", width = 0.7) +
        labs(
          title = "Carbon Footprint Contribution by Activity",
          x = "Activity",
          y = "Contribution (kg CO2)"
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_fill_brewer(palette = "Set3")
    })
  })
}

# Run the app
shinyApp(ui, server)
