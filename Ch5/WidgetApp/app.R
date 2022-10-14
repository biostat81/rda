#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
ui <- fluidPage(
  titlePanel("Basic widgets"),
  fluidRow(
    column(3,h3("Single checkbox"),
             checkboxInput("checkbox", label = "Choice A", value = TRUE)),
    column(3,selectInput("select", 
                       label = h3("Select box"), 
                       choices = list("Choice 1" = 1, 
                                      "Choice 2" = 2,
                                      "Choice 3" = 3), 
                       selected = 1)),
    column(3,checkboxGroupInput("checkGroup", 
                              label = h3("Checkbox group"), 
                              choices = list("Choice 1" = 1, 
                                             "Choice 2" = 2, "Choice 3" = 3),
                              selected = 1)),
    column(3, radioButtons("radio", label = h3("Radio buttons"),
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                                       "Choice 3" = 3),selected = 1)) 
  ),
  
  fluidRow(
    column(3,textOutput("checkboxOut")),
    column(3,textOutput("selectOut")),      
    column(3,textOutput("checkGroupOut")),
    column(3,textOutput("radioOut"))   
  ),
  fluidRow(
    column(3,sliderInput("slider1", label = h3("Slider 1"),
                       min = 0, max = 100, value = 50)),
    column(3,sliderInput("slider2", label = h3("Slider 2"),
                       min = 0, max = 100, value = c(25, 75))),
    column(3,textInput("text", label = h3("Text input"), 
                     value = "Enter text...")),   
    column(3,numericInput("num", 
                        label = h3("Numeric input"), 
                        value = 1))   
  ),
  fluidRow(
    column(3,textOutput("slider1Out")),
    column(3,textOutput("slider2Out")),
    column(3,textOutput("textOut")),
    column(3, textOutput("numOut"))   
  )
)

server <- function(input, output) {
  output$checkboxOut <- renderPrint({ input$checkbox })
  output$checkGroupOut <- renderPrint({ input$checkGroup })
  output$radioOut <- renderPrint({ input$radio })
  output$selectOut <- renderPrint({ input$select })
  
  output$slider1Out <- renderPrint({ input$slider1 })
  output$slider2Out <- renderPrint({ input$slider2 })
  output$textOut <- renderPrint({ input$text })
  output$numOut <- renderPrint({ input$num})
  
}
# Run the application 
shinyApp(ui = ui, server = server)



