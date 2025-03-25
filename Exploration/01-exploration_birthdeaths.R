####################Trying out some approaches to integrate population dynamics###################
rm(list = ls())

#----------simplified approach to implement basic birth-death dynamics:
host_availability <- function(t, current_ids, total_pop_size, birth_rate, death_rate) {
  # Calculate births
  new_births <- rpois(1, birth_rate * total_pop_size)
  new_birth_ids <- paste0("H", max(as.numeric(gsub("H", "", current_ids)), na.rm = TRUE) + 1:(new_births))
  
  # Calculate natural deaths
  if(length(current_ids) > 0) {
    natural_deaths <- rbinom(1, length(current_ids), death_rate)
    death_ids <- if (natural_deaths > 0) sample(current_ids, natural_deaths) else c()
  } else {
    death_ids <- c()
  }
  
  return(list(
    "new_hosts" = new_birth_ids,  # Returns actual IDs
    "removed_hosts" = death_ids
  ))
}

if (t %% 100 == 0) {
  print(paste("Time:", t, "Population Size:", total_pop_size, 
              "Births:", new_births, "Deaths:", length(death_ids)))
}


#---------------Nosoi homogeneous population --> implementing it 
p_Exit_fct  <- function(t){return(0.08)}

n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

p_Trans_fct <- function(t,p_max,t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

# Starting the simulation ------------------------------------
SimulationSingle <- nosoiSim(type="single", popStructure="none",
                             nLoc = 1,
                             length.sim=1000, max.infected=1000, init.individuals=1, 
                             nContact=n_contact_fct,
                             param.nContact=NA,
                             timeDep.nContact=FALSE,
                             pExit = p_Exit_fct,
                             param.pExit=NA,
                             timeDep.pExit=FALSE,
                             pTrans = p_Trans_fct,
                             param.pTrans = param_pTrans,
                             timeDep.pTrans=FALSE,
                             prefix.host="H",
                             print.progress=FALSE,
                             #add population dynamics here:
                             hostAvailability = host_availability,
                             param.hostAvailability = list(
                               birth_rate=0.05, 
                               death_rate=0.03))
###---------------------DOES NOT WORK





###---------------------link the population structure to a discrete time first

#so try that out first, a discrete-time population model
# Parameters
time_steps <- 100
initial_population <- 1000
birth_rate <- 0.05
death_rate <- 0.05

# Storage
population <- numeric(time_steps)
population[1] <- initial_population

# Simulation loop
for (t in 2:time_steps) {
  births <- rpois(1, birth_rate * population[t-1])  # Random births
  deaths <- rbinom(1, population[t-1], death_rate)  # Random deaths
  
  population[t] <- population[t-1] + births - deaths
}

# Plot population dynamics over time
plot(1:time_steps, population, type="l", col="blue", lwd=2,
     xlab="Time Steps", ylab="Population Size", main="Discrete-Time Population Dynamics")





#-------------------------------
#can we make like a similar function for this population size? that when you run it each time something different comes out of it? like a function that generates a different population size each time you run it?
simulate_population <- function(time_steps = 100, initial_population = 1000, 
                                birth_rate = 0.05, death_rate = 0.05) {
  # Storage for population size
  population <- numeric(time_steps)
  population[1] <- initial_population
  
  # Simulation loop
  for (t in 2:time_steps) {
    births <- rpois(1, birth_rate * population[t-1])
    deaths <- rbinom(1, population[t-1], death_rate)
    population[t] <- population[t-1] + births - deaths
  }
  
  # Return the population trajectory
  return(population)
}

pop_sim <- simulate_population()
plot(1:length(pop_sim), pop_sim, type="l", col="blue", lwd=2,
     xlab="Time Steps", ylab="Population Size", main="Population Simulation")



#-----------------------------link this to nContact
#Now we have created a fluctuating population size dependent on birth and deaths. So, this influences the number of contacts that can be made.
#nContact determines how many individuals an infected host interacts with per time step. Works alongside pTrans to determine how many infections actually happen. Can be fixed or dynamic, depending on host behavior, infection stage, environment etc. Higher nContacts = faster potential spread, but only if pTrans is not zero

#So, nContacts now becomes dependent on the population size. So, the bigger the population the more contacts and the smaller the fewer contacts, So, it should be some sort of percentage or something. 

#nContact --> depends on pop_sim --> modify n_contact_fct to depend on current population size
#but should everything now become dependent on the population size or is only nContact sufficient?

simulate_population <- function(time_steps, initial_population, birth_rate, death_rate) {
  population <- numeric(time_steps)
  population[1] <- initial_population
  
  for (t in 2:time_steps) {
    births <- rpois(1, birth_rate * population[t-1])
    deaths <- rbinom(1, population[t-1], death_rate)
    population[t] <- population[t-1] + births - deaths
  }
  
  return(population)}

time_steps <- 100
pop_size <- simulate_population(time_steps, initial_population = 1000, 
                               birth_rate = 0.05, death_rate = 0.05)

plot(1:length(pop_size), pop_size, type="l", col="blue", lwd=2,
     xlab="Time Steps", ylab="Population Size", main="Population Simulation")


# Modify n_contact_fct to use population size
n_contact_fct <- function(t, pop_size) {
  current_pop <- pop_size[min(t, length(pop_size))]  # Get current population size
  # Generate a contact number directly proportional to population size
  n_contacts <- abs(round(rnorm(1, current_pop / 1000, 1), 0))  
  return(n_contacts)  
  }

#but now it calculates the number of contacts for a population as a whole!! That's not what we want. In nosoi it also calculates it for each individually infected host. So, how many contacts does each individually infected host within the population make? 
n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))} #this is the original function, so this one needs to be adjusted 

#next try:
n_contact_fct <- function(t, pop_size) {
  current_pop <- pop_size[min(t, length(pop_size))]  # Get current population size
  # Generate a contact number directly proportional to population size
  n_contacts <- abs(round(rnorm(1, current_pop / 1000, 1), 0))  
  return(rep(n_contacts, current_pop))  
  }

#This function is not correct because it returns a vector of the same value repeated current_pop times. return(rep(n_contacts, current_pop)) → This creates a vector with the same contact number repeated for every individual in the population, rather than per infected individual.

n_contact_fct <- function(t, pop_sim) {
  current_pop <- pop_sim[min(t, length(pop_sim))]  # Get current population size
  base_rate <- 0.5  # Base mean contacts per individual (from original function)
  # Scale mean contacts per individual based on population size
  scaled_mean <- base_rate * (current_pop / 1000)  
  # Each infected individual gets their own contact number (stochastic)
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0))  
  return(n_contacts_i)  
}

#this should be the right fix, as each individual gets their own contact number. also maintains stochasticity, contacts fluctuate at each time step + it scales with population size. This way, Nosoi still correctly assigns individual contacts per infected host, while ensuring that contact rates scale properly with population size.

#----------------------------now trying to integrate this part into the nosoi model (example model below in this script)
n_contact_fct <- function(t) {
  current_pop <- pop_sim[min(t, length(pop_sim))]  # Get current population size
  base_rate <- 0.5  
  scaled_mean <- base_rate * (current_pop / 1000)  
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0))  
  return(n_contacts_i)  
}

# Other functions remain the same
p_Exit_fct  <- function(t) { return(0.08) }

p_Trans_fct <- function(t, p_max, t_incub) {
  if (t < t_incub) { p <- 0 }
  if (t >= t_incub) { p <- p_max }
  return(p)
}

t_incub_fct <- function(x) { rnorm(x, mean = 7, sd = 1) }
p_max_fct <- function(x) { rbeta(x, shape1 = 5, shape2 = 2) }

param_pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct)

# Run the Nosoi simulation
set.seed(805)
SimulationSingle <- nosoiSim(type = "single", popStructure = "none",
                             length.sim = time_steps, max.infected = 100, init.individuals = 1,
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

#well it does run! Now gives the same result as before, because of the set.seed
SimulationSingle <- nosoiSim(type = "single", popStructure = "none",
                             length.sim = time_steps, max.infected = 100, init.individuals = 1,
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







##-----------------------------Example model:
p_Exit_fct  <- function(t){return(0.08)}

n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

p_Trans_fct <- function(t,p_max,t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)}

t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

# Starting the simulation ------------------------------------
set.seed(805)
SimulationSingle <- nosoiSim(type="single", popStructure="none",
                             length.sim=100, max.infected=100, init.individuals=1, 
                             nContact=n_contact_fct,
                             param.nContact=NA,
                             timeDep.nContact=FALSE,
                             pExit = p_Exit_fct,
                             param.pExit=NA,
                             timeDep.pExit=FALSE,
                             pTrans = p_Trans_fct,
                             param.pTrans = param_pTrans,
                             timeDep.pTrans=FALSE,
                             prefix.host="H",
                             print.progress=FALSE)
#The simulation has run for 40 units of time and a total of 111 hosts have been infected.

library(ggplot2)
cumulative.table <- getCumulative(SimulationSingle)
dynamics.table <- getDynamic(SimulationSingle)

p1 <- ggplot(data=cumulative.table, aes(x=t, y=Count)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Time (t)",y="Cumulative count of infected hosts")

p2 <- ggplot(data=dynamics.table, aes(x=t, y=Count)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Time (t)",y="Number of active infected hosts")

#combine p1, p2 
library(patchwork)
p1 + p2 


