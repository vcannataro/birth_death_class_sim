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
                             min = 0, max = 1,
                             value = 0.2, step = 0.1),
                 sliderInput("exp_pop_init_size", "Initial population size:",
                             min = 2, max = 1000,
                             value = 100, step = 1),
                 
                 sliderInput("exp_gen_num", "Number of generations:",
                             min = 2, max = 200,
                             value = 20, step = 1),
                 
                 actionButton("sim_run_exp", "Run simulation")
               ),
               mainPanel(
                 plotOutput("exp_plot"),
                 dataTableOutput("exp_table")
               )
               
               
             ),
             
             
    ),
    
    tabPanel("Density dependent growth", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(      
                 sliderInput("logis_birth_rate", "Intrinsic birth rate: ",
                             min = 0, max = 10,
                             value = 2.3, step = 0.1),
                 
                 sliderInput("logis_death_rate", "Intrinsic death rate:",
                             min = 0, max = 2,
                             value = 0.2, step = 0.1),
                 sliderInput("logis_pop_init_size", "Initial population size:",
                             min = 2, max = 1000,
                             value = 100, step = 1),
                 
                 sliderInput("logis_gen_num", "Number of generations:",
                             min = 2, max = 200,
                             value = 20, step = 1),
                 
                 sliderInput("logis_car_capac", "Carrying capacity of the population:",
                             min = 2, max = 100000,
                             value = 1000, step = 1),
                 
                 actionButton("sim_run_logis", "Run simulation")
               ),
               mainPanel(
                 plotOutput("logis_plot"),
                 dataTableOutput("logis_table")
               )
               
               
             ),
             
             
    )
    
    
    
    
  ),
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  # initializing data structures ---- 
  sim_data_exp <- reactiveValues(data=NULL)
  sim_data_logis <- reactiveValues(data=NULL)
  
  
  # exponential ---- 
  observeEvent(input$sim_run_exp, {
    sim_data_exp$data <- data.frame(generation = 0:(input$exp_gen_num), 
                                population_size = 
                                  c(input$exp_pop_init_size,rep(NA, 
                                                                input$exp_gen_num)),
                                new_births = NA,
                                new_deaths = NA)
    
    for(gen_index in 2:nrow(sim_data_exp$data)){
      
      sim_data_exp$data[gen_index,"new_births"] <- 
        round(sim_data_exp$data[gen_index-1,"population_size"] * input$exp_birth_rate)
      sim_data_exp$data[gen_index,"new_deaths"] <- 
        round(sim_data_exp$data[gen_index-1,"population_size"] * input$exp_death_rate)
      
      new_pop_size <-  sim_data_exp$data[gen_index-1,"population_size"] + 
        sim_data_exp$data[gen_index,"new_births"]  - sim_data_exp$data[gen_index,"new_deaths"]
      
      sim_data_exp$data[gen_index,"population_size"] <- ifelse(new_pop_size > 0, new_pop_size, 0)
      
      
    }
    
    
  })
  
  output$exp_plot <- renderPlot({
    
    if (is.null(sim_data_exp$data)) return()
    
    plot(x=sim_data_exp$data$generation,
         y=sim_data_exp$data$population_size,
         type="b",
         xlab="Generation number",
         ylab="Population size")
    
    
  })
  
  output$exp_table <- renderDataTable(sim_data_exp$data,
                                      options = list(pageLength = 10,
                                                     searching = FALSE))
  
  
  # logistic ---- 
  
  observeEvent(input$sim_run_logis, {
    sim_data_logis$data <- data.frame(generation = 0:(input$logis_gen_num), 
                                    population_size = 
                                      c(input$logis_pop_init_size,rep(NA, 
                                                                    input$logis_gen_num)),
                                    change_in_population = NA)
    
    for(gen_index in 2:nrow(sim_data_logis$data)){
      
      sim_data_logis$data[gen_index,"change_in_population"] <- 
        round(sim_data_logis$data[gen_index-1,"population_size"] * 
                ((input$logis_birth_rate - input$logis_death_rate) * 
                   (1- (sim_data_logis$data[gen_index-1,"population_size"] / 
                          input$logis_car_capac)
                    )
                 )
              )

      new_pop_size <-  sim_data_logis$data[gen_index-1,"population_size"] + 
        sim_data_logis$data[gen_index,"change_in_population"]
      
      sim_data_logis$data[gen_index,"population_size"] <- ifelse(new_pop_size > 0, new_pop_size, 0)
      
      
    }
    
    
  })
  
  
  output$logis_plot <- renderPlot({
    
    if (is.null(sim_data_logis$data)) return()
    
    plot(x=sim_data_logis$data$generation,
         y=sim_data_logis$data$population_size,
         type="b",
         xlab="Generation number",
         ylab="Population size")
    
    
  })
  
  output$logis_table <- renderDataTable(sim_data_logis$data,
                                      options = list(pageLength = 10,
                                                     searching = FALSE))
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
