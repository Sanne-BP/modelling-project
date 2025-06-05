#Now we are going to run population dynamics beforehand, so outside of the core! For this we use the base version, hence this script and to make sure it actually works and other people can also use it!!

#So we will do this for singleNone + for singleDiscrete / dualDiscrete:2 populations --> if you are in A use that pop size and in B use that pop size one --> worst case scenario is 2 populations (lol)
#As soon as it works: test test test, at least 100 simulations (but perhaps like hundreds more for different parameters etc.) --> this will take time as well, we are not there yet!! --> then we can finally move forward towards the more biological, scientific part, actually focusing on a question!!


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