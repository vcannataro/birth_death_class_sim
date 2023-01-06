starting_pop <- 1000

birthrate <- 2
deathrate <- 2.1

generations <- 100


population_size_df <- data.frame(generation = 0:(generations-1), 
                                 population_size = c(starting_pop,rep(NA, 
                                                                      generations-1)),
                                 new_births = NA,
                                 new_deaths = NA)


for(gen_index in 2:nrow(population_size_df)){
  
  population_size_df[gen_index,"new_births"] <- round(population_size_df[gen_index-1,"population_size"] * birthrate)
  population_size_df[gen_index,"new_deaths"] <- round(population_size_df[gen_index-1,"population_size"] * deathrate)
  
  new_pop_size <-  population_size_df[gen_index-1,"population_size"] + 
    population_size_df[gen_index,"new_births"]  - population_size_df[gen_index,"new_deaths"]
  
  population_size_df[gen_index,"population_size"] <- ifelse(new_pop_size > 0, new_pop_size, 0)
  
  
}


plot(x=population_size_df$generation,y=population_size_df$population_size)
