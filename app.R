# Load packages ----
library(shiny)
library(quantmod)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Demonstration of Stocks During the Time"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine.

        Information will be collected from Yahoo finance."),
      textInput("symb", "Symbol", "TSLA"),
      
      dateRangeInput("dates",
                     "Date range",
                     start = "2015-01-01",
                     end = as.character(Sys.Date())),
      
      br(),
      br(),
      
      checkboxInput("log", "Plot y axis on log scale",
                    value = FALSE),
      
      checkboxInput("adjust",
                    "Adjust prices for inflation", value = FALSE)
    ),
    
    mainPanel(plotOutput("plot"))
  )
  
  ,
  fluidRow(
    column(3, 
           sliderInput("slider1", h3("Select a Number"),
                       min = 0, max = 100, value = 25),
           plotOutput("hist"),
           verbatimTextOutput("stats")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({  
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  finalInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
  })
  
  output$plot <- renderPlot({
    chartSeries(finalInput(), theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })
  
  output$hist <- renderPlot({
    hist(rnorm(input$slider1))
  })
  
  output$stats <- renderPrint({
    summary(rnorm(input$slider1))
  })
}
# Run the app
shinyApp(ui, server)
