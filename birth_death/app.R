#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Population Growth"),
  
  tabsetPanel(
    tabPanel("Density independent growth", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(      
                 sliderInput("exp_birth_rate", "Birth rate: ",
                             min = 0, max = 10,
                             value = 2.3, step = 0.1),
                 
                 sliderInput("exp_death_rate", "Death rate:",
                             min = 0, max = 10,
                             value = 2.3, step = 0.1),
                 sliderInput("exp_pop_init_size", "Initial population size:",
                             min = 2, max = 1000,
                             value = 100, step = 1),
                 
                 sliderInput("exp_gen_num", "Number of generations:",
                             min = 2, max = 1000,
                             value = 100, step = 1),
                 
                 actionButton("sim_run", "Run simulation")
               ),
               mainPanel(
                 plotOutput("exp_plot")
               )
               
               
             ),
             
             
    )
  ),
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  sim_data <- reactiveValues(data=NULL)
  
  
  observeEvent(input$sim_run, {
    sim_data$data <- data.frame(generation = 0:(input$exp_gen_num-1), 
                                population_size = 
                                  c(input$exp_pop_init_size,rep(NA, 
                                                                input$exp_gen_num-1)),
                                new_births = NA,
                                new_deaths = NA)
    
    for(gen_index in 2:nrow(sim_data$data)){
      
      sim_data$data[gen_index,"new_births"] <- round(sim_data$data[gen_index-1,"population_size"] * input$exp_birth_rate)
      sim_data$data[gen_index,"new_deaths"] <- round(sim_data$data[gen_index-1,"population_size"] * input$exp_death_rate)
      
      new_pop_size <-  sim_data$data[gen_index-1,"population_size"] + 
        sim_data$data[gen_index,"new_births"]  - sim_data$data[gen_index,"new_deaths"]
      
      sim_data$data[gen_index,"population_size"] <- ifelse(new_pop_size > 0, new_pop_size, 0)
      
      
    }
    
    
  })
  
  output$exp_plot <- renderPlot({
    
    if (is.null(sim_data$data)) return()
    
    plot(x=sim_data$data$generation,y=sim_data$data$population_size)
    
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
