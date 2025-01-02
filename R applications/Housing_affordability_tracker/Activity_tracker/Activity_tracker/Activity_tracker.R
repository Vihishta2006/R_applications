library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Activity Tracker"),
  sidebarLayout(
    sidebarPanel(
      numericInput("steps", "Steps Walked:", value = 0, min = 0),
      numericInput("calories", "Calories Burned:", value = 0, min = 0),
      actionButton("log_activity", "Log Activity")
    ),
    mainPanel(
      plotOutput("activity_scatter_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  activity_data <- reactiveVal(data.frame(Steps = numeric(), Calories = numeric()))
  
  observeEvent(input$log_activity, {
    new_entry <- data.frame(Steps = input$steps, Calories = input$calories)
    activity_data(rbind(activity_data(), new_entry))
  })
  
  output$activity_scatter_plot <- renderPlot({
    ggplot(activity_data(), aes(x = Steps, y = Calories)) +
      geom_point(color = "blue", size = 3) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(title = "Steps vs Calories Burned",
           x = "Steps Walked",
           y = "Calories Burned") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)
