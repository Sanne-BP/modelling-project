#Now we are going to run population dynamics beforehand, so outside of the core! For this we use the base version, hence this script and to make sure it actually works and other people can also use it!!

#So we will do this for singleNone + for singleDiscrete / dualDiscrete:2 populations --> if you are in A use that pop size and in B use that pop size one --> worst case scenario is 2 populations (lol)
#As soon as it works: test test test, at least 100 simulations (but perhaps like hundreds more for different parameters etc.) --> this will take time as well, we are not there yet!! --> then we can finally move forward towards the more biological, scientific part, actually focusing on a question!!

#clear environment:
rm(list = ls())

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

#Defining Parameters:
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



#-----------------------------------------------------------------------------------------------
#so NOW: make it as dependent as we did in singleNone!!
run_singlenone <- function(length.sim = 100,
                                  initial.population = 100000,
                                  birth.rate = 0.5,
                                  death.rate = 0.5,
                                  pExit = function(t) 0.04,
                                  pTrans = function(t, p_max, t_incub) if (t < t_incub) 0 else p_max,
                                  t_incub_fct = function(x) rnorm(x, mean = 5, sd = 1),
                                  p_max_fct = function(x) rbeta(x, 5, 2),
                                  base_contact_rate = 10,
                                  print.progress = FALSE) {
  
  # Store PopModel over time
  PopModel <- numeric(length.sim + 1)
  PopModel[1] <- initial.population
  
  # Contact function depends on PopModel
  n_contact_fct <- function(t) {
    current_pop <- PopModel[min(t + 1, length(PopModel))]
    scaled_mean <- base_contact_rate * (current_pop / 1000)
    abs(round(rnorm(1, scaled_mean, 1)))
  }
  
  # Param for pTrans
  param_pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct)
  
  # Run the nosoi simulation
  sim <- nosoiSim(type = "single", popStructure = "none",
                  length.sim = length.sim, max.infected = 100000, init.individuals = 1,
                  nContact = n_contact_fct, param.nContact = NA, timeDep.nContact = FALSE,
                  pExit = pExit, param.pExit = NA, timeDep.pExit = FALSE,
                  pTrans = pTrans, param.pTrans = param_pTrans, timeDep.pTrans = FALSE,
                  prefix.host = "H", print.progress = print.progress)
  
  # Extract epidemic exit times
  exit_times <- sim$history$exit
  
  for (t in 1:length.sim) {
    pop_prev <- PopModel[t]
    
    # Epidemic deaths = number of hosts exiting at time t
    epidemic_deaths <- sum(exit_times == t)
    
    # Natural demographic dynamics
    births <- rpois(1, birth.rate * pop_prev)
    deaths <- rbinom(1, pop_prev, death.rate)
    
    # Update population
    PopModel[t + 1] <- max(0, pop_prev + births - deaths - epidemic_deaths)
    
    # Early exit if epidemic has died out
    if (t >= sim$total.time) {
      PopModel <- PopModel[1:(t + 1)]
      break
    }
  }
  
  return(list(simulation = sim, pop_model = PopModel))
}

res <- run_singlenone(length.sim = 100,
                             initial.population = 100000,
                             birth.rate = 0.5,
                             death.rate = 0.5,
                             print.progress = TRUE)

# Plot population over time
pop_df <- data.frame(
  Time = seq_along(res$pop_model) - 1,
  PopSize = res$pop_model
)

# Plot
p1 <- ggplot(pop_df, aes(x = Time, y = PopSize)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Population Dynamics", x = "Time", y = "Population Size") +
  theme_minimal()

p1

#so what do I have now: PopModel linked to natural births and deaths + epidemic deaths (pExit). nContact dependent ONLY on the initital PopModel values (which start with initital.population and get updated inly after the full simulation), so no feedback in between!! So during the epidemic simulation itself, contact rates are fixed based on initital Popmodel, no intra-simulation feedback
#this is also a result, show!!!!

#Starting the simulation
#Initializing ... running ...
#Time: 10 (10% of maximum length). Hosts count: 7 (0% of maximum infected hosts).
#Time: 20 (20% of maximum length). Hosts count: 42 (0% of maximum infected hosts).
#Time: 30 (30% of maximum length). Hosts count: 249 (0% of maximum infected hosts).
#Time: 40 (40% of maximum length). Hosts count: 1329 (1% of maximum infected hosts).
#Time: 50 (50% of maximum length). Hosts count: 6854 (7% of maximum infected hosts).
#Time: 60 (60% of maximum length). Hosts count: 34987 (35% of maximum infected hosts).
#done. 
#The simulation has run for 67 units of time and a total of 108844 hosts have been infected.

library(patchwork)

#epidemic outcomes:
res$simulation$total.time
res$simulation$type

getHostData(res$simulation, what = "N.infected", pop = "A")
getTableHosts(res$simulation, pop = "A")
summary(res$simulation)

cumulative.table <- getCumulative(res$simulation)
cumulative.table$Type <- "Cumulative Infected"

dynamics.table <- getDynamic(res$simulation)
dynamics.table$Type <- "Active Infected"
#combine these 2 into 1 table

combined_df <- bind_rows(cumulative.table, dynamics.table)

p2 <- ggplot(combined_df, aes(x = t, y = Count, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Cumulative vs Active Infected",
       x = "Time (t)",
       y = "Number of Hosts",
       color = NULL) +
  theme_minimal()

p2

p1 + p2 + plot_layout(ncol = 1)

data = data.frame(R0=getR0(res$simulation)$R0.dist)
ggplot(data=data, aes(x=R0)) + geom_histogram() + theme_minimal()
