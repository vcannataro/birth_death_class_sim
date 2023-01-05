library(shiny)

# from https://bookdown.org/janengelstaedter/biol3360modelling3/discrete-time-models-in-one-variable.html


#source("Apps/ExpLogApp/plotGrowth.R")

plotExp <-  function(rate = 2, size = 10) {
  time <- seq(0, 20, 0.1)
  
  par(mar = c(5, 5, 0, 0), oma = c(0, 0, 1, 1), mgp = c(2.5, 1, 0))
  plot(x = time, y = rate^time*size, type = "l", col = "red", 
       ylab = "Population Size", xlab = "Time", cex.lab = 2, lwd = 2)
}

plotLog <-  function(rate = 2, size = 10, maxsize = 40) {
  time <- seq(0, 100, 1)
  
  y <- vector(length = length(time))
  y[1] <- size/maxsize
  for (i in 2:length(time)) {
    y[i] <- rate * (1 - y[i - 1]) * y[i - 1]
  }
  par(mar = c(5, 5, 0, 0), oma = c(0, 0, 1, 1), mgp = c(2.5, 1, 0))
  plot(x = time, y = y, ylim = c(0, 1), type = "l", col = "red", 
       ylab = "Pop Relative to Maximum", xlab = "Time", cex.lab = 2, lwd = 2)
}

app <- shinyApp(
  ui = fluidPage(
    titlePanel("Exponential vs. Logistic Growth"),
    
    tabsetPanel(
      tabPanel("Exponential", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(      
                   sliderInput("rate1", "Growth Rate:",
                               min = 0.5, max = 4.5,
                               value = 2, step = 0.1),
                   
                   sliderInput("size1", "Initial Population Size:",
                               min = 5, max = 50, value = 10) 
                 ),
                 
                 mainPanel(
                   plotOutput("exp")
                 )
               )
      ),
      
      tabPanel("Logarithmic", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(      
                   sliderInput("rate2", "Growth Rate:",
                               min = 0.5, max = 4.5,
                               value = 2, step = 0.1),
                   
                   sliderInput("size2", "Initial Population Size:",
                               min = 5, max = 50, value = 10), 
                   
                   sliderInput("max", "Maximum Population Size:",
                               min = 5, max = 100, value = 40) 
                 ),
                 
                 mainPanel(
                   plotOutput("log")
                 )
               )
      )
    )
  ),
  
  server = function(input, output) {  
    output$exp <- renderPlot({
      plotExp(rate = input$rate1, size = input$size1)
    })
    
    output$log <- renderPlot({
      plotLog(rate = input$rate2, size = input$size2, maxsize = input$max)
    })
  }
)

# Run the app ----
runApp(app)