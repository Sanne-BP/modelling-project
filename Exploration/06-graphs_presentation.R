#Here I will be making some graphs / results for the presentation!!

#-----------------------------------------------------------------------------------------------
#PopModel:
time_steps <- 120

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

pop_size <- simulate_population(time_steps, initial_population = 5000, 
                                birth_rate = 0.3, death_rate = 0.25)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Dynamics") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18))

#save plot:
ggsave("Plots/Presentation/PopModel_only3.png", width = 8, height = 4, dpi = 300, bg = "white")
















#-----------------------------------------------------------------------------------------------
#making number of contacts dependent on PopModel:
p_Exit_fct  <- function(t) { return(0.04) }

p_Trans_fct <- function(t, p_max, t_incub) {
  if (t < t_incub) { p <- 0 }
  if (t >= t_incub) { p <- p_max }
  return(p)
}

t_incub_fct <- function(x) { rnorm(x, mean = 5, sd = 1) }
p_max_fct <- function(x) { rbeta(x, shape1 = 5, shape2 = 2) }

param_pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct)

#------
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

time_steps <- 100
pop_size <- simulate_population(time_steps, initial_population = 100000, 
                                birth_rate = 0.4, death_rate = 0.4)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size), 
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

#adjusting number of contacts
n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t + 1, length(pop_size))]
  base_rate <- 5  # more conservative for rodents
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

#plotting:
sim_times <- 0:12        #specify per simulation!!
contacts_sim <- sapply(sim_times, n_contact_fct)
pop_sim <- pop_size[sim_times + 1]  # because pop_size[1] = time 0

df <- data.frame(Time = sim_times,
                 Population = pop_sim,
                 Contacts = contacts_sim)

p1 <- ggplot(df, aes(x = Time)) +
  geom_line(aes(y = Population, color = "Population Size"), size = 1) +
  geom_line(aes(y = Contacts * 200, color = "Contacts per Individual"), size = 1) +
  scale_color_manual(values = c("Population Size" = "blue", "Contacts per Individual" = "red")) +
  scale_y_continuous(
    name = "Population size",
    sec.axis = sec_axis(~ . / 200, name = "Number of contacts")
  ) +
  labs(title = "Population Dynamics",
       x = "Time", color=NULL) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),)
p1


#Saving this image and lets try it for multiple runs!!
ggsave("Plots/Presentation/Popsize2.png", width = 8, height = 4, dpi = 300, bg = "white")







