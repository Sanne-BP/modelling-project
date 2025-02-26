#############Simulation set up
rm(list = ls())

#########Spread of a pathogen in a homogeneous population
#nosoi can accommodate a wide range of epidemiological transmission scenarios. It hence relies on many parameters, that need to be set properly for the right scenario to be simulated. This tutorial aims to illustrate how to set up a nosoi simulation for a “simple” case: a pathogen being transmitted within a population without structure. We will present two cases, first for a single-host, and then a dual-host pathogen.

#Google docs link for explanation of all functions/options/parameters:
#https://docs.google.com/document/d/1KgfNs9LIZh3C33dI75nKRRYTw70rzpz16HkiB_hLLpg/edit?tab=t.0

######Setting up the simulation
#Setting up a basic pathogen spread simulation in a homogeneous population with no structured environment. Function nosoiSim is the main wrapper for running simulations in nosoi, in this case we use singleNone "single host, no structure" setup. So we start by providing the options type="single" and popStructure="none". 
SimulationSingle <- nosoiSim(type="single", popStructure="none", ...)

#General parameters
SimulationSingle <- nosoiSim(type="single", popStructure="none",
                             length.sim=300, max.infected=1000, init.individuals=1, ...)

#In the case of a dual host simulation, several parameters of the nosoiSim will have to be specified for each host type, designated by A and B. The wrapper function will then take all the arguments that will be passed down to the simulator.
SimulationDual <- nosoiSim(type="dual", popStructure="none", ...)





################RUNNING NOSOI

######Single host
######Simple simulation for a single host pathogen
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





