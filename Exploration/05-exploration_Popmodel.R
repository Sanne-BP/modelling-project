#So, the population size dynamics (PopModel) I have made is sufficient for now and we will work with that to implement it into Nosoi!!! 

#Now, the first step is the PopModel and the table. We have to create a table in which you will have the population number per time --> similar to the movement table of pMove, so check this!!! In the table we will start of with just the total population number at first, make a core!!!

rm(list = ls())
library(devtools)
library(nosoi)
library(ggplot2)
library(dplyr)

#PopModel:
time_steps <- 1000

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

pop_size <- simulate_population(time_steps, initial_population = 100000, 
                                birth_rate = 0.5, death_rate = 0.5)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

#Parameters:
n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t + 1, length(pop_size))]  
  base_rate <- 10  
  scaled_mean <- base_rate * (current_pop / 1000)  
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0)) 
  return(n_contacts_i)  
}

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
#The simulation has run for 12 units of time and a total of 1321 hosts have been infected.




#-------------------------------------------------------------------------------------------------
#Lets see what kind of tables we can extract from this simulation
SimulationSingle$total.time   # [1] 12
SimulationSingle$type         # [1] "single"

SimulationSingle$host.info.A
getHostData(SimulationSingle, "table.hosts")
#getHostData(SimulationSingle, "table.state"), does not work as there is no state information kept when the host population A has no structure

#Sebastian did specifically mention the "movement table" in the last meeting, but that would require adding a structure to my PopModel and Simulation, so lets see if we can work with the other table.hosts table first?
getHostData(SimulationSingle, "table.hosts")
getTableHosts(SimulationSingle, "A")

#convert to data frame
table.hosts <- as.data.frame(getTableHosts(SimulationSingle, "A"))











#-------------------------------------------------------------------------------------------------
#Create a table which stores the population number per time step
pop_size_table <- data.frame(
  time = 0:time_steps,
  population = pop_size  )

#we adjusted the popsize dynamics to start at t=0 instead of at t=1, but now the table looks quite weird, but it should be correct this way

#maybe we can also add the number of contacts per time step to this table
nContact <- sapply(0:time_steps, n_contact_fct)

#combine into one table
table.PopModel <- data.frame(
  time = 0:time_steps,
  population = pop_size,
  nContact = nContact)

head(table.PopModel)

#maybe we can make the length of the table dependent on the length of the simulation, so it should vary each time the simulation is running. Now creating this table based on the previous simulation in which the simulation has run for 6 units of time and a total of 1790 hosts have been infected.
sim_length <- SimulationSingle$total.time

sim_times <- 0:sim_length
pop_sim <- pop_size[sim_times + 1]
contacts_sim <- sapply(sim_times, n_contact_fct)

# Create the table
table.PopModel <- data.frame(
  time = sim_times,
  population = pop_sim,
  nContact = contacts_sim)

#Well, this is the idea we are going for, so that's nice
#     time population nContact
#1    0     100000     1000
#2    1     100168     1001
#3    2      99857      998
#4    3     100092     1001
#5    4      99900      998
#6    5     100150     1002

#But now, 



















#wrapper function that tracks and stores pop size and nContacts for each step during the simulation: CHATGPT gave this, but did not try it out yet
run_sim_with_pop_tracking <- function(time_steps, 
                                      initial_population = 100000,
                                      birth_rate = 0.5, 
                                      death_rate = 0.5,
                                      contact_base_rate = 10,
                                      ...) {
  
  # Simulate the population size dynamics
  pop_size <- simulate_population(time_steps, initial_population, birth_rate, death_rate)
  
  # Function to compute number of contacts based on current population
  n_contact_fct <- function(t) {
    current_pop <- pop_size[min(t + 1, length(pop_size))]  # Time shift for indexing
    scaled_mean <- contact_base_rate * (current_pop / 1000)
    n_contacts <- abs(round(rnorm(1, scaled_mean, 1), 0))
    return(n_contacts)
  }
  
  # Create the contact vector using the contact function
  sim_times <- 0:(length(pop_size) - 1)
  contacts_sim <- sapply(sim_times, n_contact_fct)
  
  # Combine into a population tracking table
  table.PopModel <- data.frame(
    time = sim_times,
    population = pop_size,
    contacts = contacts_sim
  )
  
  # Run the Nosoi simulation with your custom nContact function
  SimulationSingle <- nosoiSim(
    type = "single", popStructure = "none",
    length.sim = time_steps, max.infected = 10000,
    init.individuals = 1,
    nContact = n_contact_fct, param.nContact = NA, timeDep.nContact = FALSE,
    pExit = p_Exit_fct, param.pExit = NA, timeDep.pExit = FALSE,
    pTrans = p_Trans_fct, param.pTrans = param_pTrans, timeDep.pTrans = FALSE,
    prefix.host = "H", print.progress = FALSE
  )
  
  # Attach the population model table to the simulation object
  SimulationSingle$popSize.table <- table.PopModel
  
  return(SimulationSingle)
}
















#so how to make the population size dependent on nosoi?















