####################Trying out some approaches to integrate population dynamics###################

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


###---------------------DOES NOT WORK,FIX THIS!!!!










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





#can we make like a similar function for this population size? that when you run it each time something different comes out of it?
#like a function that generates a different population size each time you run it?
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

plot(1:length(pop_sim), simulate_population(), type="l", col="blue", lwd=2,
     xlab="Time Steps", ylab="Population Size", main="Population Simulation")



#-----------------------------link this to nContact













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

