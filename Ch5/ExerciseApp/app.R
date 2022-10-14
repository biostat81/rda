
library(shiny)

ui <-fluidPage(
  titlePanel("Histogram"),
  sidebarLayout( 
    sidebarPanel(
      selectInput(inputId = "Xvar", 
                  label = "X variable",
                  choices = colnames(iris),
                  selected = colnames(iris)[1]),
      textInput(inputId = "Mainvar", label = "Main title",
                value = "enter main title"),
      textInput(inputId = "Xlab", label = "X label",
                value= "enter X lable")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {

  output$distPlot <- renderPlot({
    x  <- iris[, input$Xvar] 
    hist(x,main=input$Mainvar,xlab=input$Xlab)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
