#In this script we do the following:
#Let the population dynamics start at t = 0, as the nosoi simulation also starts from t = 0, right now my population dynamics start at t = 1!
#After that, try to model it with more extreme parameters to actually see if it works. For now I have been keeping everything very small and realistic, this way changes cannot even be seen.
             #This has all been done successfully in this script!!

rm(list = ls())
library(devtools)
library(nosoi)
library(ggplot2)
library(dplyr)

#Population dynamics:
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


##------------------------------------------------------------------------------------------------
##Now adjusting to let the population dynamics start at t = 0, as the nosoi simulation also starts from t = 0, right now my population dynamics start at t = 1!

simulate_population <- function(time_steps, initial_population, birth_rate, death_rate) {
  population <- numeric(time_steps + 1)         # Add one extra slot for t = 0
  population[1] <- initial_population           # Now population[1] corresponds to t = 0
  
  for (t in 2:(time_steps + 1)) {
    births <- rpois(1, birth_rate * population[t - 1])
    deaths <- rbinom(1, population[t - 1], death_rate)
    population[t] <- population[t - 1] + births - deaths
  }
  
  return(population)
}

pop_size <- simulate_population(time_steps, initial_population = 1000, 
                                birth_rate = 0.5, death_rate = 0.5)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t + 1, length(pop_size))]  # shift index by 1
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


#Checking if it works!
head(pop_size)   # Should start at t = 0
#[1] 1000 1002 1007 1006 1018 1023
length(pop_size) # Should be time_steps + 1
#[1] 1001

#seems like it works!!

ggplot(data = data.frame(Time = 0:time_steps, Population = pop_size), 
       aes(x = Time, y = Population)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Population Dynamics (starting at t = 0)",
       x = "Time", y = "Population Size")








##------------------------------------------------------------------------------------------------
#Now, try to model it with more extreme parameters to actually see if it works. For now I have been keeping everything very small and realistic, this way changes cannot even be seen.
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

time_steps <- 1000
pop_size <- simulate_population(time_steps, initial_population = 1000000, 
                                birth_rate = 0.9, death_rate = 0.9)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

#adjusting number of contacts
n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t + 1, length(pop_size))]  # shift index by 1
  base_rate <- 20 
  scaled_mean <- base_rate * (current_pop / 1000)  
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0)) 
  return(n_contacts_i)  
}

#Other functions remain the same
SimulationSingle <- nosoiSim(type = "single", popStructure = "none",
                             length.sim = time_steps, max.infected = 10000, 
                             init.individuals = 1,
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
#The simulation has run for 5 units of time and a total of 11034 hosts have been infected.

#So, lets see if it works and plot it!
sim_times <- 0:5        #as this specific simulation only ran for 5 units 
contacts_sim <- sapply(sim_times, n_contact_fct)
pop_sim <- pop_size[sim_times + 1]  # because pop_size[1] = time 0

ggplot(data = data.frame(Time = sim_times, Population = pop_sim, 
                         Contacts = contacts_sim), aes(x = Time)) +
  geom_line(aes(y = Population, color = "Population Size"), linewidth = 1) +
  geom_line(aes(y = Contacts, color = "Number of Contacts"), linewidth = 1) +
  theme_minimal() +
  labs(title = "Population Dynamics and Number of Contacts",
       x = "Time", y = "Value") +
  scale_color_manual(values = c("Population Size" = "blue", 
                                "Number of Contacts" = "red")) +
  theme(legend.title = element_blank())

#> head(pop_sim)
#[1] 1000000 1001320 1000457  999942  997362  997496
#> head(contacts_sim)
#[1] 19998 20026 20010 19997 19947 19949
#This looks promising!! But both values are large and only slightly fluctuating across to their value, so therefore they might look like straight values when plotting them in the same plot

#try plotting them on separate axis:
df <- data.frame(Time = sim_times,
                 Population = pop_sim,
                 Contacts = contacts_sim)

p1 <- ggplot(df, aes(x = Time)) +
  geom_line(aes(y = Population), color = "blue", size = 1) +
  geom_line(aes(y = Contacts * 50), color = "red", size = 1) +  # scaled for comparison
  scale_y_continuous(
    name = "Population Size",
    sec.axis = sec_axis(~ . / 50, name = "Number of Contacts")  # reverse scale
  ) +
  labs(title = "Population Dynamics and Number of Contacts (base rate=20)",
       x = "Time") +
  theme_minimal()
p1

#Well, this looks great!!! So this means its actually working. 
#It kinda does look to good to be true, but lets test this
#Saving this image and lets try it for multiple runs!!
ggsave("Plots/Fig_Test_Popsize&Contacts.pdf", width = 8, height = 6)




#-----------------------------------------------------------------------------------------------
#Now, lets try it for multiple runs!!
time_steps <- 1000
pop_size <- simulate_population(time_steps, initial_population = 1000000, 
                                birth_rate = 0.9, death_rate = 0.9)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t + 1, length(pop_size))]  # shift index by 1
  base_rate <- 1 
  scaled_mean <- base_rate * (current_pop / 1000)  
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0)) 
  return(n_contacts_i)  
}

#Other functions remain the same
SimulationSingle <- nosoiSim(type = "single", popStructure = "none",
                             length.sim = time_steps, max.infected = 10000, 
                             init.individuals = 1,
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
#The simulation has run for 8 units of time and a total of 12598 hosts have been infected.

sim_times <- 0:8        #as this specific simulation only ran for 9 units 
contacts_sim <- sapply(sim_times, n_contact_fct)
pop_sim <- pop_size[sim_times + 1]  # because pop_size[1] = time 0

d5 <- data.frame(Time = sim_times,
                 Population = pop_sim,
                 Contacts = contacts_sim)

p6 <- ggplot(d5, aes(x = Time)) +
  geom_line(aes(y = Population), color = "blue", size = 1) +
  geom_line(aes(y = Contacts * 1000), color = "red", size = 1) + 
  scale_y_continuous(name = "Population Size",
    sec.axis = sec_axis(~ . / 1000, name = "Number of Contacts")) +
  scale_x_continuous(limits = c(0, 9), breaks = seq(0, 9, 1)) +
  labs(title = "Population Dynamics and Number of Contacts (base rate=1)",
       x = "Time") +
  theme_minimal()
p6


#combining these plots
p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 2)
ggsave("Plots/Fig_Testcombined_Popsize&Contacts.pdf", width = 12, height = 10)
#this looks amazing!! with base rate = 1, the number of contacts fluctuate less but that is completely logical, as only 0.5% is then the nContact of the entire population of 1 million


#------------------------------------------------------------------------------------------------
#Now some tries with different birth rates and death rates
time_steps <- 1000
pop_size <- simulate_population(time_steps, initial_population = 1000000, 
                                birth_rate = 0.2, death_rate = 0.2)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t + 1, length(pop_size))]  # shift index by 1
  base_rate <- 5
  scaled_mean <- base_rate * (current_pop / 1000)  
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0)) 
  return(n_contacts_i)  
}

#Other functions remain the same
SimulationSingle <- nosoiSim(type = "single", popStructure = "none",
                             length.sim = time_steps, max.infected = 10000, 
                             init.individuals = 1,
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
#The simulation has run for 5 units of time and a total of 32656 hosts have been infected.

sim_times <- 0:5        #as this specific simulation only ran for 9 units 
contacts_sim <- sapply(sim_times, n_contact_fct)
pop_sim <- pop_size[sim_times + 1]  # because pop_size[1] = time 0

d10 <- data.frame(Time = sim_times,
                 Population = pop_sim,
                 Contacts = contacts_sim)

p10 <- ggplot(d10, aes(x = Time)) +
  geom_line(aes(y = Population), color = "blue", size = 1) +
  geom_line(aes(y = Contacts * 200), color = "red", size = 1) + 
  scale_y_continuous(name = "Population Size",
                     sec.axis = sec_axis(~ . / 200, name = "Number of Contacts")) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
  labs(title = "(b=0.2 & d=0.2)",
       x = "Time") +
  theme_minimal()
p10

#combining these plots
p3 + p4 + 
  p7 + p8 +
  p9 + p10 +
  plot_layout(ncol = 2)
ggsave("Plots/Fig_Testcombined_Popsize&Contacts_birthdeathrate.pdf", width = 12, height = 10)

#Also looks really good!!!



#------------------------------------------------------------------------------------------------
#Now I only want to see a longer simulation, after that I am content
time_steps <- 1000
pop_size <- simulate_population(time_steps, initial_population = 100000, 
                                birth_rate = 0.5, death_rate = 0.5)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t + 1, length(pop_size))]  # shift index by 1
  base_rate <- 5
  scaled_mean <- base_rate * (current_pop / 1000)  
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0)) 
  return(n_contacts_i)  
}

#Other functions remain the same
SimulationSingle <- nosoiSim(type = "single", popStructure = "none",
                             length.sim = time_steps, max.infected = 100000, 
                             init.individuals = 1,
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

#The simulation has run for 10 units of time and a total of 3121733 hosts have been infected.
#this takes a while to load with such a high number of max.infected, so in this case not so nice for testing to use max.infected of 1 million. and because the base_rate is quite "high", the max infected is reached very quickly. But that's also because the population is really big, so you reach it faster anyway
#NO MORE max.infected = >1.000.000!!! takes too damn long (also i am impatient lol)

#The simulation has run for 20 units of time and a total of 15629 hosts have been infected.
#The simulation has run for 11 units of time and a total of 12218 hosts have been infected.
#The simulation has run for 16 units of time and a total of 17608 hosts have been infected.
#The simulation has run for 17 units of time and a total of 179085 hosts have been infected.
#The simulation has run for 11 units of time and a total of 254150 hosts have been infected.

sim_times <- 0:11     
contacts_sim <- sapply(sim_times, n_contact_fct)
pop_sim <- pop_size[sim_times + 1]  # because pop_size[1] = time 0

d16 <- data.frame(Time = sim_times,
                  Population = pop_sim,
                  Contacts = contacts_sim)

p16 <- ggplot(d16, aes(x = Time)) +
  geom_line(aes(y = Population), color = "blue", size = 1) +
  geom_line(aes(y = Contacts * 200), color = "red", size = 1) + 
  scale_y_continuous(name = "Population Size",
                     sec.axis = sec_axis(~ . / 200, name = "Number of Contacts")) +
  scale_x_continuous(limits = c(0, 11), breaks = seq(0, 11, 1)) +
  labs(title = "(baserate = 5, max.infected = 100.000, popsize = 100000)",
       x = "Time") +
  theme_minimal()
p16


#combining the different tries plots
p11 + p12 + p13 + p14 + p15 + p16 +
  plot_layout(ncol = 2)
ggsave("Plots/Fig_Testcombined_Popsize&Contacts_difftries.pdf", width = 12, height = 10)

#I think it is safe to say, that nContacts are actually being extracted from the population size dynamics. So that's really good! Now, we can move forward from this.

