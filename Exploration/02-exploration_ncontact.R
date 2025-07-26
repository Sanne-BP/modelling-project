#Modify nContact function at first, have epidemiology influence contact
#--> not the other way around yet, later the population can influence epidemiology
#--> how does it influence fitness, how does that influence the probability to create offspring? 
#       o	A sicker host might have fewer offspring (lower birth rate) 
#       o	Highly transmissible virus could kill hosts faster, reducing the overall birth rate 
#       o	A host population that collapses due to infection would lead to pathogen extinction 

#Perhaps its possible to integrate some parameters for nContact
#no idea how, but lets see!

##-----------------------------Example Nosoi model:
p_Exit_fct  <- function(t){return(0.08)}

n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

p_Trans_fct <- function(t,p_max,t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)}

t_incub_fct <- function(x){rnorm(x,mean = 3,sd=1)} 
#lower incubation time, so transmission begins faster
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

# Starting the simulation ------------------------------------
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





#is it possible to plot the nContacts? NO idea how to do that, but perhaps we can see a pattern with the population size, as those two things are the only things that are now linked to each other. This continues in script 02-exploration_ncontact
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

library(dplyr)
View(contact_log)






#the contact_log dataframe does not make a lot of sense right now, so instead of logging one row per infected individual, we can sum all contacts per time step.
contact_log <- data.frame(Time = numeric(), Population = numeric(), Contacts = numeric())

n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t, length(pop_size))]  # Get current population size
  base_rate <- 0.5  
  scaled_mean <- base_rate * (current_pop / 1000)  
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0))  
  
  # If time step already exists, sum contacts; otherwise, add new row
  if (t %in% contact_log$Time) {
    contact_log[contact_log$Time == t, "Contacts"] <<- contact_log[contact_log$Time == t, "Contacts"] + n_contacts_i
  } else {
    contact_log <<- rbind(contact_log, data.frame(Time = t, Population = current_pop, Contacts = n_contacts_i))
  }
  
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
#The simulation has run for 37 units of time and a total of 1042 hosts have been infected.


#perhaps we can see a clearer trend now!
p_c <- ggplot(contact_log, aes(x = Population, y = Contacts)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(x = "Population Size", y = "Number of Contacts", 
       title = "Contacts vs. Population Size")


#now we should plot the pop dynamics only from time steps 0 to 37
p_p <- ggplot(data = data.frame(Time = 1:37, Population = pop_size[1:37]), 
       aes(x = Time, y = Population)) +
  geom_line(color = "blue", linewidth = 0.5) +
  theme_minimal() +
  labs(x = "Time Steps", y = "Population Size", title = "Population Size Dynamics (First 37 Steps)")

#combine these into 1 plot:
p_c + p_p + plot_layout(ncol = 1)


#also extracting cumulative + dynamics data
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

p_x <- ggplot(combined_data) +
  geom_line(aes(x=Time, y=ActiveInfected, color="Active Infected"), linewidth=1) +
  geom_line(aes(x=Time, y=CumulativeInfected, color="Cumulative Infected"), linewidth=1) +
  labs(x="Time (t)", y="Number of Infected Hosts", title="Active vs Cumulative Infections") +
  scale_color_manual(values=c("red", "blue")) +
  theme_minimal()


#combine these into 1 plot:
p_c + p_p + p_x + plot_layout(ncol = 1)














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




