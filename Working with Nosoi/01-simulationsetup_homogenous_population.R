#############Simulation set up
rm(list = ls())

#########Spread of a pathogen in a homogeneous population
#nosoi can accommodate a wide range of epidemiological transmission scenarios. It hence relies on many parameters, that need to be set properly for the right scenario to be simulated. This tutorial aims to illustrate how to set up a nosoi simulation for a “simple” case: a pathogen being transmitted within a population without structure. We will present two cases, first for a single-host, and then a dual-host pathogen.

#Google docs link for explanation of all functions/options/parameters:
#https://docs.google.com/document/d/1KgfNs9LIZh3C33dI75nKRRYTw70rzpz16HkiB_hLLpg/edit?tab=t.0

#-------Setting up the simulation
#Setting up a basic pathogen spread simulation in a homogeneous population with no structured environment. Function nosoiSim is the main wrapper for running simulations in nosoi, in this case we use singleNone "single host, no structure" setup. So we start by providing the options type="single" and popStructure="none". 
SimulationSingle <- nosoiSim(type="single", popStructure="none", ...)

#-------General parameters
SimulationSingle <- nosoiSim(type="single", popStructure="none",
                             length.sim=300, max.infected=1000, init.individuals=1, ...)

#In the case of a dual host simulation, several parameters of the nosoiSim will have to be specified for each host type, designated by A and B. The wrapper function will then take all the arguments that will be passed down to the simulator.
SimulationDual <- nosoiSim(type="dual", popStructure="none", ...)





################RUNNING NOSOI

######Single host
#-------Simple simulation for a single host pathogen
p_Exit_fct  <- function(t){return(0.08)} 
#Contstant value os 0.08 is chosen, which means an infected host has 8% chance to                  leave the simulation at each unit of time

#pExit, like the other core functions, has to be a function of t, even if t is not used. Since pExit is constant here, there is no use for the "absolute" time of the simulation nor for the individual-based parameters. That is because the probability of an infected host leaving the simulation does not change over time - it is always 8% per time step. So param.pExit=NA, and timeDep.pExit=FALSE.

n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}
#A constant function is chosen that will draw a value from a normal distribution with mean=0.5 and sd=1, round it (thats what the 0 is for), and take its absolute value (so that the number of contacts is never negative)

#At each time step and for each infected host, nContact will be drawn anew. 
#Remember that nContact, like the other core functions also has to be a function of t, even if t is not used. Since nContact is constant here, there is no use for the “absolute” time of the simulation nor for the individual-based parameters. So param .nContact=NA, and timeDep.nContact=FALSE.

p_Trans_fct <- function(t, p_max, t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}
#We choose pTrans in the form of a threshold function: before a certain amount of time since initial infection, the host does not transmit (incubation time, which we call t_incub), and after that time it will transmit with a certain (constant) probability (which we call p_max). This function is dependent on the time since the host’s infection t:

t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
#Because each host is different (slightly different biotic and abiotic factors), you can expect each host to exhibit differences in the dynamics of infection, and hence the probability of transmission over time. Thus, t_incub and p_max will be sampled for each host individually according to a certain distribution. t_incub will be sampled from a normal distribution of mean=7 and sd= 1, while p_max will be sampled from a beta distribution with shape parameters 𝛼=5 and𝛽=2

    #Plotting distribution of max transmission probability (p_max):
      #Generate 10,000 samples
      p_max_samples <- rbeta(10000, shape1 = 5, shape2 = 2)

      #Plot a histogram
     hist(p_max_samples, 
     breaks = 30, 
     col = "lightgreen", 
     main = "Distribution of Maximum Transmission Probability (p_max)", 
     xlab = "p_max", 
     ylab = "Frequency", 
     border = "white")

      #Add a vertical line for the mean
      abline(v = mean(p_max_samples), col = "red", lwd = 2, lty = 2)

      #Most p_max values will be greater than 0.5, meaning hosts have a relatively high chance of        transmitting the pathogen after the incubation period.

#Note that here t_incub and p_max are functions of x and not t (they are not core functions but individual-based parameters), and x enters the function as the number of draws to make.

#pTrans is not dependent on the “absolute” time of the simulation, so timeDep.pTrans=FALSE. However, since we make use of individual-based parameters, we have to provide a param.pTrans as a list of functions. The name of each element within this list should have the same name that the core function (here pTrans) uses as argument
param_pTrans = list(p_max=p_max_fct, t_incub=t_incub_fct)




####RUNNING
#Once nosoiSim is set up, you can run the simulation (here the “seed” ensures that you will obtain the same results as in this tutorial):
library(nosoi)  

#pExit
p_Exit_fct  <- function(t){return(0.08)}

#nContact
n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
p_Trans_fct <- function(t,p_max,t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

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
#Once the simulation has finished, it reports the number of time units for which the simulation has run (40), and the maximum number of infected hosts (111). Note that the simulation has stopped here before reaching length.sim as it has crossed the max.infected threshold set at 100.






######Dual host
#Setting up a dual host simulation is similar to the single host version described above, but each parameter has to be provided for both hosts. Here, we choose for Host A the same parameters as the single / only host above. Host B will have sightly different parameters:

p_Exit_fctB  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16} #for a periodic function
#for pExit.B a value is chosen that depends on the "absolute" time of the simulation, for example cyclic climatec conditions (temperature). In that case, the function's argumens should be t and prestime (the "absolute" time of the simulation)

#Since pExit.B is dependent on the simulation’s absolute time, do not forget to set timeDep.pExit.B to TRUE. Since there are no individual-based parameters, param.pExit.B=NA.

n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}
#For nContact.B, we choose a constant function that will sample a value out of a provided range of possible values, each with a certain probability. So, each value here has as specific probability. This means that most infected hosts will not make any new contacts, some will infect one person, and only a few will infect two people.

#At each time and for each infected host, nContact.B will be drawn anew. Remember that nContact.B, like the other core functions has to be function of t, even if t is not used. Since nContact.B is constant here, there is no use for the “absolute” time of the simulation nor for the individual-based parameters. So param.nContact.B=NA, and timeDep.nContact.B=FALSE.

p_Trans_fct.B <- function(t, max.time){
  dnorm(t, mean=max.time, sd=2)*5
}
#We choose pTrans.B in the form of a Gaussian function. It will reach its maximum value at a certain time point (mean) after initial infection and will subsequently decrease until it reaches 0. dnorm() calculates the probability density of a normal distribution. mean=max.time is peak transmission time for this specific host. sd=2 determines how quickly the probability drops before and after max.time. *5 scales the probability so that the peak reaches a mmore reasonable value for transmission probability

max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}
#Because each host is different (slightly different biotic and abiotic factors), you can expect each host to exhibit differences in the dynamics of infection, and hence the probability of transmission over time. Thus, max.time will be sampled for each host individually according to a certain distribution. max.time will be sampled from a normal distribution of parameters mean=5 and sd = 1. So each host's peak tranmission occurs at a different time, but most will peak around 5 time units after infection, with some variation
#Note again that here max.time is a function of x and not t (not a core function but individual-based parameters), and x enters the function as the number of draws to make.

param_pTrans.B = list(max.time=max.time_fct)
#Since pTrans.B is not dependent on the “absolute” time of the simulation, timeDep.pTrans.B=FALSE. It depends on the host's own timeline and not the overall simulation time.
#However, since we make use of individual-based parameters, we have to provide a param.pTrans as a list of functions. The name of each element of the list should have the same name as the core function (here pTrans.B) uses as argument, as shown here



####RUNNING
#Once nosoiSim is set up, you can run the simulation (here the “seed” ensures that you will obtain the same results as in this tutorial):

library(nosoi)

#HostA ------------------------------------

#pExit
p_Exit_fct.A  <- function(t){return(0.08)}

#nContact
n_contact_fct.A = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
p_Trans_fct.A <- function(t,p_max,t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans.A = list(p_max=p_max_fct,t_incub=t_incub_fct)

#Host B ------------------------------------

#pExit
p_Exit_fct.B  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16}

#nContact
n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}

#pTrans
p_Trans_fct.B <- function(t, max.time){
  dnorm(t, mean=max.time, sd=2)*5
}

max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}

param_pTrans.B = list(max.time=max.time_fct)

# Starting the simulation ------------------------------------

set.seed(606)
SimulationDual <- nosoiSim(type="dual", popStructure="none",
                           length.sim=100, 
                           max.infected.A=100, 
                           max.infected.B=100, 
                           
                           init.individuals.A=1, 
                           init.individuals.B=0, 
                           
                           nContact.A=n_contact_fct.A,
                           param.nContact.A=NA,
                           timeDep.nContact.A=FALSE,
                           pExit.A=p_Exit_fct.A,
                           param.pExit.A=NA,
                           timeDep.pExit.A=FALSE,
                           pTrans.A=p_Trans_fct.A,
                           param.pTrans.A=param_pTrans.A,
                           timeDep.pTrans.A=FALSE,
                           prefix.host.A="H",
                           
                           nContact.B=n_contact_fct.B,
                           param.nContact.B=NA,
                           timeDep.nContact.B=FALSE,
                           pExit.B=p_Exit_fct.B,
                           param.pExit.B=NA,
                           timeDep.pExit.B=TRUE,
                           pTrans.B=p_Trans_fct.B,
                           param.pTrans.B=param_pTrans.B,
                           timeDep.pTrans.B=FALSE,
                           prefix.host.B="V",
                           
                           print.progress=FALSE)

#The simulation has run for 43 units of time and a total of 101 (A) and 92 (B) hosts have been infected.
#Once the simulation has finished, it reports the number of time units for which the simulation has run (43), and the maximum number of infected hosts A (101) and hosts B (92). Note that the simulation has stopped here before reaching length.sim as it has crossed the max.infected.A threshold set at 100.

#This mirrors real-world outbreaks, where a new pathogen might start in one species (Host A) before spilling over into another species (Host B). This setup lets us study cross-species transmission dynamics (e.g., how likely the disease is to jump from Host A to Host B).








#---------------------------------Trying to Visualize---------------------------------------------

library(ggplot2)
install.packages("dplyr")
library(dplyr)

# Combine Host A and Host B infection times
df_infected <- bind_rows(
  data.frame(time = SimulationDual$host.info.A$table.hosts$inf.time, Host = "A"),
  data.frame(time = SimulationDual$host.info.B$table.hosts$inf.time, Host = "B")
)

# Count new infections per time step
df_incidence <- df_infected %>%
  count(time, Host)

# Plot
ggplot(df_incidence, aes(x = time, y = n, color = Host)) +
  geom_line() +
  geom_point() +
  labs(title = "Incidence Over Time", x = "Time", y = "New Infections") +
  scale_color_manual(values = c("A" = "blue", "B" = "red")) +
  theme_minimal()
















#-------------------------------------------Altering the simulation-------------------------------

######Single host
library(nosoi)  

#pExit
p_Exit_fct  <- function(t){return(0.001)}

#nContact
n_contact_fct = function(t){abs(round(rnorm(1, 10, 1), 0))}

#pTrans
p_Trans_fct <- function(t,p_max,t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

t_incub_fct <- function(x){rnorm(x,mean = 2,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 10,shape2=2)}

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

#The simulation has run for 9 units of time and a total of 1 hosts have been infected.
SimulationSingle$host.info$table.hosts
#Means that no hosts were infected or recorded during the simulation. This suggests that the infection died out immediately, likely due to that the initital host exited too quickly, there were no contact events or the transmission probability was too low.
SimulationSingle$host.info$N.infected
n_contact_fct(1)  # Run it multiple times to see what values it returns
p_max_fct(1)  # Generate a sample p_max
t_incub_fct(1)  # Generate a sample incubation time

#The outcome of your simulation depends on the interplay between transmission probability (pTrans), contact rate (nContact), and exit probability (pExit), along with random variation in each of these parameters. This explains why in one run, you got 105 infections and in another, only 1 infection—small changes in individual factors (due to random sampling) can lead to vastly different epidemic dynamics.

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









######Dual host
#HostA ------------------------------------

#pExit
p_Exit_fct.A  <- function(t){return(0.08)}

#nContact
n_contact_fct.A = function(t){abs(round(rnorm(1, 5, 1), 0))}

#pTrans
p_Trans_fct.A <- function(t,p_max,t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans.A = list(p_max=p_max_fct,t_incub=t_incub_fct)

#Host B ------------------------------------

#pExit
p_Exit_fct.B  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16}

#nContact
n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}

#pTrans
p_Trans_fct.B <- function(t, max.time){
  dnorm(t, mean=max.time, sd=2)*5
}

max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}

param_pTrans.B = list(max.time=max.time_fct)

# Starting the simulation ------------------------------------
SimulationDual <- nosoiSim(type="dual", popStructure="none",
                           length.sim=100, 
                           max.infected.A=1000, 
                           max.infected.B=1000, 
                           
                           init.individuals.A=1, 
                           init.individuals.B=0, 
                           
                           nContact.A=n_contact_fct.A,
                           param.nContact.A=NA,
                           timeDep.nContact.A=FALSE,
                           pExit.A=p_Exit_fct.A,
                           param.pExit.A=NA,
                           timeDep.pExit.A=FALSE,
                           pTrans.A=p_Trans_fct.A,
                           param.pTrans.A=param_pTrans.A,
                           timeDep.pTrans.A=FALSE,
                           prefix.host.A="H",
                           
                           nContact.B=n_contact_fct.B,
                           param.nContact.B=NA,
                           timeDep.nContact.B=FALSE,
                           pExit.B=p_Exit_fct.B,
                           param.pExit.B=NA,
                           timeDep.pExit.B=TRUE,
                           pTrans.B=p_Trans_fct.B,
                           param.pTrans.B=param_pTrans.B,
                           timeDep.pTrans.B=FALSE,
                           prefix.host.B="V",
                           
                           print.progress=FALSE)






#-----------------------------------------------
#####THIS DOES NOT WORK whoops
#Try running multiple simulations and summarize these results
# Function to run a single simulation
run_simulation <- function() {
  sim <- nosoiSim(
    type="single", popStructure="none",
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
    print.progress=FALSE
  )
  
  # Check if the simulation actually resulted in infections
  if (is.null(sim$host.info$N.infected) || sim$host.info$N.infected == 1) {
    return(data.frame(total_infected = NA, duration = NA))
  }
  
  # Extract key results
  total_infected <- sim$host.info$N.infected
  duration <- sim$total.time
  
  return(data.frame(total_infected, duration))
}

# Run multiple simulations
n_runs <- 100
results <- do.call(rbind, replicate(n_runs, run_simulation(), simplify=FALSE))

# Remove NA rows
results <- na.omit(results)

# Summary of results
summary(results)

# Visualize total infections
ggplot(results, aes(x=total_infected)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7) +
  labs(title="Distribution of Total Infected Hosts", x="Total Infected", y="Frequency")

# Visualize duration of outbreaks
ggplot(results, aes(x=duration)) +
  geom_histogram(binwidth=5, fill="red", alpha=0.7) +
  labs(title="Distribution of Epidemic Duration", x="Simulation Duration", y="Frequency")
