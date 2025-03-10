#exploring some things together with ChatGPT
#clear environment
rm(list=ls())

#---------set up population dynamics
#Parameters
population_size <- 1000  # Initial population size
birth_rate <- 0.02  # Birth rate (constant)
death_rate <- 0.01  # Death rate (constant)
time_steps <- 100  # Number of simulation steps (e.g., years)

#Vector to store population size at each time step
pop_size <- numeric(time_steps)

#Run the simulation
for (t in 1:time_steps) {
  # Births and deaths
  births <- round(population_size * birth_rate)
  deaths <- round(population_size * death_rate)
  
  #Update population size
  population_size <- population_size + births - deaths
  
  #Store the population size at this time step
  pop_size[t] <- population_size}

# Plot population over time
plot(1:time_steps, pop_size, type = "l", col = "blue", 
     xlab = "Time Step", ylab = "Population Size", main = "Population Dynamics")



#---------introduce virus transmission
# Parameters for virus transmission
infection_rate <- 0.2  # Probability of infection per interaction
recovery_rate <- 0.05  # Rate at which infected recover

# Initial state
susceptible <- population_size - 1  # One infected individual to start
infected <- 1
recovered <- 0

# Initialize storage vectors
infected_data <- numeric(time_steps)  
susceptible_data <- numeric(time_steps)  

# Simulation of the epidemic
for (t in 1:time_steps) {
  # Infection process (basic example)
  new_infections <- round(susceptible * infected * infection_rate / population_size)
  new_recoveries <- round(infected * recovery_rate)
  
  # Update numbers
  susceptible <- susceptible - new_infections
  infected <- infected + new_infections - new_recoveries
  recovered <- recovered + new_recoveries
  
  # Store data
  infected_data[t] <- infected
  susceptible_data[t] <- susceptible}

# Plot epidemic over time
plot(1:time_steps, infected_data, type = "l", col = "red", 
     xlab = "Time Step", ylab = "Number of Infected Individuals", main = "Epidemic Spread")

