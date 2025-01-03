library(shiny)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Expense Tracker"),
  sidebarLayout(
    sidebarPanel(
      textInput("item", "Item Name:"),
      numericInput("cost", "Cost:", value = 0),
      actionButton("add_expense", "Add Expense")
    ),
    mainPanel(
      h3("Expense Log"),
      dataTableOutput("expense_table"),
      h3("Total Expense:"),
      verbatimTextOutput("total_expense")
    )
  )
)

# Server
server <- function(input, output, session) {
  expenses <- reactiveVal(data.frame(Item = character(), Cost = numeric()))
  
  observeEvent(input$add_expense, {
    new_entry <- data.frame(Item = input$item, Cost = input$cost, stringsAsFactors = FALSE)
    expenses(rbind(expenses(), new_entry))
  })
  
  output$expense_table <- renderDataTable({
    datatable(expenses())
  })
  
  output$total_expense <- renderText({
    sum(expenses()$Cost)
  })
}

# Run the app
shinyApp(ui, server)
