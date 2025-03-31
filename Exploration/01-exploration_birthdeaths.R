####################Trying out some approaches to integrate population dynamics###################
rm(list = ls())

#----------simplified approach to implement basic birth-death dynamics:
###---------------------DOES NOT WORK
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

pop_size <- simulate_population()
plot(1:length(pop_size), pop_size, type="l", col="blue", lwd=2,
     xlab="Time Steps", ylab="Population Size", main="Population Simulation")



#-----------------------------link this to nContact
#Now we have created a fluctuating population size dependent on birth and deaths. So, this influences the number of contacts that can be made.
#nContact determines how many individuals an infected host interacts with per time step. Works alongside pTrans to determine how many infections actually happen. Can be fixed or dynamic, depending on host behavior, infection stage, environment etc. Higher nContacts = faster potential spread, but only if pTrans is not zero

#So, nContacts now becomes dependent on the population size. So, the bigger the population the more contacts and the smaller the fewer contacts, So, it should be some sort of percentage or something. 

#nContact --> depends on pop_size --> modify n_contact_fct to depend on current population size
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

n_contact_fct <- function(t, pop_size) {
  current_pop <- pop_size[min(t, length(pop_size))]  # Get current population size
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
  current_pop <- pop_size[min(t, length(pop_size))]  # Get current population size
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

#Well, it does successfully run, but it is of course not clearly visual if it is actually working.
#Trying to visualize:
cumulative.table <- getCumulative(SimulationSingle)
ggplot(data=cumulative.table, aes(x=t, y=Count)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Time (t)",y="Cumulative count of infected hosts")

dynamics.table <- getDynamic(SimulationSingle)
ggplot(data=dynamics.table, aes(x=t, y=Count)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Time (t)",y="Number of active infected hosts")

p3 <- ggplot(data=data.frame(Time=1:length(pop_size), 
                             Population=pop_size), aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

# Combine both cumulative and active infected data for comparison
combined_data <- data.frame(
  Time = dynamics.table$t,
  ActiveInfected = dynamics.table$Count,
  CumulativeInfected = cumulative.table$Count
)

p4 <- ggplot(combined_data) +
  geom_line(aes(x=Time, y=ActiveInfected, color="Active Infected"), linewidth=1) +
  geom_line(aes(x=Time, y=CumulativeInfected, color="Cumulative Infected"), linewidth=1) +
  labs(x="Time (t)", y="Number of Infected Hosts", title="Active vs Cumulative Infections") +
  scale_color_manual(values=c("red", "blue")) +
  theme_minimal()

library(patchwork)
p3 + p4 + plot_layout(ncol=1)
ggsave("Plots/Fig_exploration_birthdeaths.pdf", width = 8, height = 6, dpi = 300)


#try some other visualizations
summary(SimulationSingle)

library(ggplot2)
library(viridis)
library(igraph)
library(ggnetwork)
library(ggpubr)

#network:
data.sim <- getTableHosts(SimulationSingle, "A")
graph.simA <- graph.data.frame(data.sim[-1,c("inf.by","hosts.ID")],directed=T,vertices = data.sim)
graph.simA.network <- ggnetwork(graph.simA, layout = with_kk()) 

ggplot(graph.simA.network, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey70",arrow = arrow(length = unit(0.3, "lines"), type = "open")) +
  geom_nodes(aes(color=inf.time)) + 
  scale_color_viridis(name="Time of infection",option = "plasma") + 
  theme_blank()

#phylogenetic tree:
library(ggtree)
test.nosoiA.tree <- getTransmissionTree(SimulationSingle) #Extraction of the full transmission tree from the simulated data

ggtree(test.nosoiA.tree) + geom_nodepoint(aes(color=state)) + geom_tippoint(aes(color=state)) + 
  theme_tree2() + xlab("Time (t)") + theme(legend.position = c(0,0.8), 
                                           legend.title = element_blank(),
                                           legend.key = element_blank()) 














#-----------------------------now lets try to compare multiple runs with varying parameters
#lets try different birth/death rates or transmission probabilities to compare dynamics
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

p10 <- ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")
p10

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

#visualize:
cumulative.table <- getCumulative(SimulationSingle)
ggplot(data=cumulative.table, aes(x=t, y=Count)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Time (t)",y="Cumulative count of infected hosts")

dynamics.table <- getDynamic(SimulationSingle)
ggplot(data=dynamics.table, aes(x=t, y=Count)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Time (t)",y="Number of active infected hosts")

combined_data <- data.frame(
  Time = dynamics.table$t,
  ActiveInfected = dynamics.table$Count,
  CumulativeInfected = cumulative.table$Count)

p11 <- ggplot(combined_data) +
  geom_line(aes(x=Time, y=ActiveInfected, color="Active Infected"), linewidth=1) +
  geom_line(aes(x=Time, y=CumulativeInfected, color="Cumulative Infected"), linewidth=1) +
  labs(x="Time (t)", y="Number of Infected Hosts", title="Active vs Cumulative Infections") +
  scale_color_manual(values=c("red", "blue")) +
  theme_minimal()

library(patchwork)
p10 + p11 + plot_layout(ncol=1)
ggsave("Plots/Fig_TEST3.1_exploration_birthdeaths.pdf", width = 8, height = 6, dpi = 300)


#okay the nosoi simulation is now set that it stops when 1000 hosts are infected. However, not sure whether this now actually matches with the population size dynamics. Of course currently infected does not necessarily mean that the whole population should at that moment be 0, but it should be at least 1000 right?. S0 when at time unit 45 there are already 1020 hosts infected, there should be at least 1000 individuals in the population, but that does not match, NO it does actually match never mind (only in that specific example lol). But I still don't think that the 2 are completely intertwined already, which is of course logical we are only just getting started lolzzz.
#--> SO right now only contacts are being determined by the population size dynamics that we created, but the entire simulation does not depend on the population size dynamics. 


#Let's look at example 3: so, in this test example 3 (see figure 3) the simulation has run for 40 units and 1093 hosts have been infected, lets see how big the population size is at that very time step
p12 <- ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
              aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) + 
  geom_vline(xintercept=40, linetype="dashed", 
             color="red", linewidth=0.8) +  
  geom_point(data=data.frame(Time=40, Population=pop_size[40]), 
             aes(x=Time, y=Population), 
             color="red", size=3) +  # Highlight the exact population size at t = 40
  annotate("text", x=42, y=pop_size[40] + 10, 
           label=paste("t = 40\nPop =", pop_size[40]), color="red", hjust=0) + # Annotate value
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", 
       title="Population Size Dynamics with t = 40 Highlighted")

#Soooo, at the time point that there are 1093 host infected, the population size is 932. So it does not match. So that would be of course the goal, to actually have it match. Which is the goal of the research project hahaha.













#-------------------------------
#is it possible to plot the nContacts? NO idea how to do that, but perhaps we can see a pattern with the population size, as those two things are the only things that are now linked to each other 
pop_size <- simulate_population(time_steps, initial_population = 1000, 
                                birth_rate = 1, death_rate = 1)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

contact_log <- data.frame(Time = numeric(), Population = numeric(), Contacts = numeric())

n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t, length(pop_size))]  # Get current population size
  base_rate <- 0.5  
  scaled_mean <- base_rate * (current_pop / 1000)  
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0)) 
  contact_log <<- rbind(contact_log, data.frame(Time = t, Population = current_pop, 
                                                Contacts = n_contacts_i))
  return(n_contacts_i)  
}

#(keeping all other parameters the same)
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

ggplot(contact_log, aes(x = Population, y = Contacts)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(x = "Population Size", y = "Number of Contacts", title = "Contacts vs. Population Size")

#of course the population size is not fluctuating THAT much to actually see a clear trend in the number of contacts vs the population size. and using linear model is also not really capturing the trend that well. 

ggplot(contact_log, aes(x = Population, y = Contacts)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(x = "Population Size", y = "Number of Contacts", title = "Contacts vs. Population Size")
#ggsave("Plots/Fig_TEST2_nContacts.pdf", width = 8, height = 6, dpi = 300)








#NOT SURE IF THIS IS CORRECT
#-------------------try this with multiple simulations

# Number of simulations
num_sims <- 50

# Store results
multi_contact_log <- data.frame(Simulation = integer(), Time = integer(), Population = numeric(), Contacts = numeric())

for (sim in 1:num_sims) {
  # Reset contact log for each simulation
  contact_log <- data.frame(Time = numeric(), Population = numeric(), Contacts = numeric())
  
  # Run population dynamics
  pop_size <- simulate_population(time_steps, initial_population = 1000, birth_rate = 0.05, death_rate = 0.05)
  
  # Define nContact function for this run
  n_contact_fct <- function(t) {
    current_pop <- pop_size[min(t, length(pop_size))]  
    base_rate <- 0.5  
    scaled_mean <- base_rate * (current_pop / 1000)  
    n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0)) 
    contact_log <<- rbind(contact_log, data.frame(Time = t, Population = current_pop, Contacts = n_contacts_i))
    return(n_contacts_i)  
  }
  
  # Run Nosoi simulation
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
  
  # Store results
  contact_log$Simulation <- sim
  multi_contact_log <- rbind(multi_contact_log, contact_log)
}

#visualize
ggplot(multi_contact_log, aes(x = Population, y = Contacts, color = factor(Simulation))) +
  geom_point(alpha = 0.3) +  
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Population Size", y = "Number of Contacts",
       title = "Contacts vs. Population Size Across Simulations") +
  theme(legend.position = "none")  # Hide legend if too many simulations

ggplot(multi_contact_log, aes(x = Time, y = Population, color = factor(Simulation))) +
  geom_line(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Time Steps", y = "Population Size", title = "Population Size Across Simulations")












#-------------------------------------------------------------------------------------------------
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