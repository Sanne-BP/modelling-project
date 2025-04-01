#Right now we have: a working simulation in which nContact is determined based on population size dynamics. But population size dynamics are not being influenced by the simulation itself, they are not dependent on each other yet. 
#     •	So we have a working population dynamics system, in which there is a given population size          which is influenced over time by birth and death rates
    # --> simplified model: population size is influenced by birth and death rates
#     •	nContact is based on this population size, each individual in the simulation gets their           own contact number 

rm(list = ls())
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






#################################################################################################
#--------------------also want to use the logistic equation to model population dynamics
#dN/dT = rN(1-N/K), with N = population size, r = intrinsic growth rate, K = carrying capacity

simulate_population_logistic <- function(time_steps, initial_population, r, K) {
  population <- numeric(time_steps)
  population[1] <- initial_population
  
  for (t in 2:time_steps) {
    growth <- r * population[t-1] * (1 - population[t-1] / K)  # Logistic growth term
    population[t] <- max(1, round(population[t-1] + growth))  # Ensure positive pop size
  }
  
  return(population)
}

# Parameters
time_steps <- 100
initial_population <- 1000
r <- 0.1  # Intrinsic growth rate
K <- 1500  # Carrying capacity

# Simulate population
pop_sizelog <- simulate_population_logistic(time_steps, initial_population, r, K)

# Plot the results
ggplot(data = data.frame(Time = 1:time_steps, Population = pop_sizelog), 
       aes(x = Time, y = Population)) +
  geom_line(color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(x = "Time Steps", y = "Population Size", title = "Logistic Population Growth")

#So, now growth slows down as population nears the carrying capacity. BUT in this case there are no random births/deaths, as it is deterministic, that's not what we want. Also there is a smooth growth curve, instead of random fluctuations. Not so excited about this. 

#--> with the same parameters, you'll always get the same result while running it. Is that something we want? No right?


#-----------------------------Adding stochasticity
simulate_population_logistic_stochastic <- function(time_steps, initial_population, r, K, variability = 0.3) {
  population <- numeric(time_steps)
  population[1] <- initial_population
  
  for (t in 2:time_steps) {
    # Logistic growth rate
    expected_growth <- r * population[t-1] * (1 - population[t-1] / K)
    
    # Stochastic component: Poisson-distributed fluctuations in growth
    growth <- rpois(1, lambda = max(1, expected_growth)) - 
      rpois(1, lambda = max(1, variability * abs(expected_growth)))
    
    # Ensure population remains positive
    population[t] <- max(1, population[t-1] + growth)
  }
  return(population)
}


time_steps <- 100
initial_population <- 1000
r <- 3
K <- 1100

# Run the stochastic model
pop_size_stochastic <- simulate_population_logistic_stochastic(time_steps, initial_population, r, K, variability = 0.3)

# Plot the stochastic population dynamics
ggplot(data = data.frame(Time = 1:time_steps, Population = pop_size_stochastic), 
       aes(x = Time, y = Population)) +
  geom_line(color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(x = "Time Steps", y = "Population Size", title = "Stochastic Logistic Growth")

#not too sure about this to be honest, I liked the simplified model more, as its more intuitive 
#there could still be a fair chance that I am doing this absolutely wrong tho












