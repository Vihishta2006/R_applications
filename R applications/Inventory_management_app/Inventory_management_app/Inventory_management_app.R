library(shiny)
library(DT)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Inventory Management"),
  sidebarLayout(
    sidebarPanel(
      textInput("product", "Product Name:"),
      numericInput("stock", "Stock:", value = 0),
      actionButton("add_product", "Add Product")
    ),
    mainPanel(
      h3("Product Inventory"),
      dataTableOutput("inventory_table"),
      h3("Total Products:"),
      verbatimTextOutput("total_products"),
      h3("Visualizations"),
      tabsetPanel(
        tabPanel("Bar Graph", plotOutput("bar_plot")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value for inventory data
  inventory <- reactiveVal(data.frame(Product = character(), Stock = numeric()))
  
  # Add new product to inventory
  observeEvent(input$add_product, {
    new_entry <- data.frame(Product = input$product, Stock = input$stock, stringsAsFactors = FALSE)
    inventory(rbind(inventory(), new_entry))
  })
  
  # Render inventory table
  output$inventory_table <- renderDataTable({
    datatable(inventory())
  })
  
  # Calculate total products
  output$total_products <- renderText({
    nrow(inventory())
  })
  
  # Render bar graph
  output$bar_plot <- renderPlot({
    inv_data <- inventory()
    ggplot(inv_data, aes(x = Product, y = Stock, fill = Product)) +
      geom_bar(stat = "identity", width = 0.7) +
      labs(title = "Inventory Stock by Product", x = "Product", y = "Stock") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "Set3")
  })
  
  # Render scatter plot
  output$scatter_plot <- renderPlot({
    inv_data <- inventory()
    ggplot(inv_data, aes(x = Product, y = Stock)) +
      geom_point(size = 4, color = "blue") +
      labs(title = "Scatter Plot of Inventory Stock", x = "Product", y = "Stock") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the app
shinyApp(ui, server)
