#
# Código de prueba


library(shiny)
library(ggplot2)

datasets <- c("economics", "faithful", "seals")
# Define UI 
ui <- fluidPage(
  
  selectInput(
    inputId = "dataset",
    label = "Conjunto de datos",
    choices = datasets
  ),
  
  verbatimTextOutput("resumen"),
  plotOutput("grafica")
)

# Define server logic 
server <- function(input, output, session) {
  
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  
  output$resumen <- renderPrint({
    summary(dataset())
  })
  
  output$grafica <- renderPlot({
    plot(dataset())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
