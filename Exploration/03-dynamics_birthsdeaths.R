#Right now we have: a working simulation in which nContact is determined based on population size dynamics. But population size dynamics are not being influenced by the simulation itself, they are not dependent on each other yet. 
#     •	So we have a working population dynamics system, in which there is a given population size          which is influenced over time by birth and death rates
#     •	nContact is based on this population size, each individual in the simulation gets their own         contact number 

library(devtools)
library(nosoi)
library(ggplot2)
library(dplyr)

time_steps <- 1000

simulate_population <- function(time_steps, initial_population, birth_rate, death_rate) {
  population <- numeric(time_steps)
  population[1] <- initial_population
  
  for (t in 2:time_steps) {
    births <- rpois(1, birth_rate * population[t-1])
    deaths <- rbinom(1, population[t-1], death_rate)
    population[t] <- population[t-1] + births - deaths
  }
  
  return(population)}

pop_size <- simulate_population(time_steps, initial_population = 1000, 
                                birth_rate = 0.5, death_rate = 0.5)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
              aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t, length(pop_size))]  # Get current population size
  base_rate <- 0.5  
  scaled_mean <- base_rate * (current_pop / 1000)  
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0))  
  return(n_contacts_i)  
}

# Other functions remain the same
p_Exit_fct  <- function(t) { return(0.04) }

p_Trans_fct <- function(t, p_max, t_incub) {
  if (t < t_incub) { p <- 0 }
  if (t >= t_incub) { p <- p_max }
  return(p)
}

t_incub_fct <- function(x) { rnorm(x, mean = 5, sd = 1) }
p_max_fct <- function(x) { rbeta(x, shape1 = 5, shape2 = 2) }

param_pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct)

# Run the Nosoi simulation
SimulationSingle <- nosoiSim(type = "single", popStructure = "none",
                             length.sim = time_steps, max.infected = 1000, init.individuals = 1,
                             nContact = n_contact_fct,
                             param.nContact = NA,
                             timeDep.nContact = FALSE,
                             pExit = p_Exit_fct,
                             param.pExit = NA,
                             timeDep.pExit = FALSE,
                             pTrans = p_Trans_fct,
                             param.pTrans = param_pTrans,
                             timeDep.pTrans = FALSE,
                             prefix.host = "H",
                             print.progress = FALSE)



