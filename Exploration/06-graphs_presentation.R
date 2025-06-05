#Here I will be making some graphs / results for the presentation!!

#PopModel:
time_steps <- 120

simulate_population <- function(time_steps, initial_population, birth_rate, death_rate) {
  population <- numeric(time_steps + 1)         
  population[1] <- initial_population           
  
  for (t in 2:(time_steps + 1)) {
    births <- rpois(1, birth_rate * population[t - 1])
    deaths <- rbinom(1, population[t - 1], death_rate)
    population[t] <- population[t - 1] + births - deaths
  }
  
  return(population)
}

pop_size <- simulate_population(time_steps, initial_population = 5000, 
                                birth_rate = 0.3, death_rate = 0.25)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Dynamics") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18))

#save plot:
ggsave("Plots/Presentation/PopModel_only3.png", width = 8, height = 4, dpi = 300, bg = "white")



