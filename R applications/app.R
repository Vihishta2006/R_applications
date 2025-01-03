# Load libraries
library(shiny)
library(DT)


# UI
ui <- fluidPage(
  titlePanel("Plants Management System"),
  tabsetPanel(
    tabPanel("Plants", DT::dataTableOutput("plants")),
    tabPanel("Add Plant", 
             textInput("plant_name", "Name"),
             selectInput("plant_type", "Type", c("Flower", "Vegetable", "Fruit", "Herb")),
             numericInput("plant_quantity", "Quantity", 1, 100),
             actionButton("add_plant", "Add")
    ),
    tabPanel("Update Plant", 
             DT::dataTableOutput("update_plants"),
             textInput("update_plant_name", "Name"),
             numericInput("update_plant_quantity", "Quantity", 1, 100),
             actionButton("update_plant", "Update")
    ),
    tabPanel("Delete Plant", 
             DT::dataTableOutput("delete_plants"),
             textInput("delete_plant_name", "Name"),
             actionButton("delete_plant", "Delete")
    )
  )
)


# Server
server <- function(input, output) {
  plants <- reactiveValues(data = data.frame(PlantName = character(), 
                                             PlantType = character(), 
                                             Quantity = numeric()))
  
  output$plants <- DT::renderDataTable({plants$data})
  
  observeEvent(input$add_plant, {
    plants$data <- rbind(plants$data, 
                         data.frame(PlantName = input$plant_name, 
                                    PlantType = input$plant_type, 
                                    Quantity = input$plant_quantity))
  })
  
  output$update_plants <- DT::renderDataTable({plants$data})
  
  observeEvent(input$update_plant, {
    plants$data[plants$data$PlantName == input$update_plant_name, 
                "Quantity"] <- input$update_plant_quantity
  })
  
  output$delete_plants <- DT::renderDataTable({plants$data})
  
  observeEvent(input$delete_plant, {
    plants$data <- plants$data[plants$data$PlantName != input$delete_plant_name, ]
  })
}


# Run App
shinyApp(ui = ui, server = server)



