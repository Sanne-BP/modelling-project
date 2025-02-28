#############Simulation set up
rm(list = ls())

############Spread of a pathogen in a discrete structured population
#Aside from the simple simulation set up, where hosts are “not structured”, nosoi can take into account a population structured either in discrete states or in a continuous space. We focus here on a discrete state structure.
#The discrete structure is intended to allow the simulation to take place in a geographical context with discrete states such as countries, regions, islands, cities. Note that other kind of structures could be taken into account (e.g. high risk/low risk, etc). In this setting, parameter values are allowed to change according to the host’s current location among the available states.
#This tutorial focuses on setting up a nosoi simulation for a pathogen which host population is structured between different locations.

#--------Structure of the population
#We consider here a population of hosts that are in three different locations called “A”, “B” and “C”. Hosts can move (if they undergo a movement event) between these locations with a certain probability that can be set directly, or derived from other data. Here, we take the transition matrix, called here after structure.matrix, to be:
#>     A   B   C
#> A 0.0 0.5 0.5
#> B 0.2 0.0 0.8
#> C 0.4 0.6 0.0


#--------Setting up the simulation
SimulationSingle <- nosoiSim(type="single", popStructure="discrete", ...)
#The wrapper function nosoiSim takes all the arguments that will be passed down to the simulator, in the case of this tutorial singleDiscrete (for “single host, discrete structure”). We thus start by providing the options type="single" and popStructure="discrete" to set up the analysis:


#--------General parameters
SimulationSingle <- nosoiSim(type="single", popStructure="none",
                             length.sim=300, max.infected=1000, init.individuals=1, ...)
#Here, we will run a simulation starting with 1 individual, for a maximum of 1,000 infected individuals and a maximum time of 300 days.


#--------Dual host
SimulationDual <- nosoiSim(type="dual", popStructure="discrete", ...)
#In the case of a dual host simulation, several parameters of the nosoiSim will have to be specified for each host type, designated by A and B. The wrapper function nosoiSim will then take all the arguments that will be passed down to the simulator, in the case of this tutorial dualDiscrete (for “dual host, discrete structure”). We thus start by providing the options type="dual" and popStructure="discrete" to set up the analysis


#####################RUNNING NOSOI
#--------Single host
#We present here a very simple simulation for a single host pathogen.

p_Exit_fct  <- function(t,current.in){
  if(current.in=="A"){return(0.02)}
  if(current.in=="B"){return(0.05)}
  if(current.in=="C"){return(0.1)}}
#we choose a probability that depends on the location where the host currently resides. Each location (state) has to be present, as shown in this example. 
#This function indicates that if the host is in state “A”, it has 2% chance to exit, 5% if in state “B” and 10% if in state “C”. Remember that pExit, like the other core functions, has to be function of t, even if t is not used. Since pExit is dependent on the location, diff.pExit=TRUE. However, there is no use of the “absolute” time of the simulation nor individual-based parameters, hence timeDep.pExit=FALSE and param.pExit=NA.

p_Move_fct  <- function(t){return(0.1)}
#We choose a constant value for pMove, namely 0.1, i.e. an infected host has 10% chance to leave its state (here a location) for each unit of time
#Remember that pMove, like the other core functions, has to be a function of t, even if t is not used. Since pMove is not dependent on the location, diff.pMove=FALSE. Similarly, there is no use of the “absolute” time of the simulation nor individual-based parameters, hence timeDep.pMove=FALSE and param.pMove=NA.

n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}
#For nContact, we choose a constant function that will draw a value in a normal distribution of mean 0.5 and sd 1, round it, and take its absolute value.
#At each time and for each infected host, nContact will be drawn anew. Remember that nContact, like the other core functions has to be function of t, even if t is not used. Since nContact is constant here, there is no use of the “absolute” time of the simulation, the location of the host, nor individual-based parameters. So param.nContact=NA, timeDep.nContact=FALSE and diff.nContact=FALSE.

p_Trans_fct <- function(t, p_max, t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)}
#We choose pTrans in the form of a threshold function: before a certain amount of time since initial infection, the host does not transmit (incubation time, which we call t_incub), and after that time, it will transmit with a certain (constant) probability (which we call p_max). This function is dependent of the time since the host’s infection t

t_incub_fct <- function(x){rnorm(x, mean=7, sd=1)}
p_max_fct <- function(x){rbeta(x, shape1=5, shape2=2)}
#Because each host is different (slightly different biotic and abiotic factors), you can expect each host to exhibit differences in the dynamics of infection, and hence the probability of transmission over time. Thus, t_incub and p_max will be sampled for each host individually according to a certain distribution. t_incub will be sampled from a normal distribution of mean=7 and sd= 1, while p_max will be sampled from a beta distribution with shape parameters 𝛼= 5 and𝛽=2
    #Note that here t_incub and p_max are functions of x and not t (they are not core functions but     individual-based parameters), and x enters the function as the number of draws to make.

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)
#pTrans is not dependent of the “absolute” time of the simulation nor it is dependent of the hosts location, hence timeDep.pTrans=FALSE and diff.pTrans=FALSE. However, since we make use of individual-based parameters, we have to provide a param.pTrans as a list of functions. The name of each element within this list should have the same name that the core function (here pTrans) uses as argument


#--------Running
library(nosoi)

#Transition matrix
transition.matrix <- matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),nrow = 3, ncol = 3,dimnames=list(c("A","B","C"),c("A","B","C")))

#pExit
p_Exit_fct  <- function(t,current.in){
  if(current.in=="A"){return(0.02)}
  if(current.in=="B"){return(0.05)}
  if(current.in=="C"){return(0.1)}
}

#pMove
p_Move_fct  <- function(t){return(0.1)}

#nContact
n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
proba <- function(t,p_max,t_incub){
  if(t <= t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

# Starting the simulation ------------------------------------

set.seed(846)
SimulationSingle <- nosoiSim(type="single", popStructure="discrete",
                             length.sim=300, max.infected=300, init.individuals=1, init.structure="A", 
                             
                             structure.matrix=transition.matrix,
                             
                             pExit = p_Exit_fct,
                             param.pExit=NA,
                             timeDep.pExit=FALSE,
                             diff.pExit=TRUE,
                             
                             pMove = p_Move_fct,
                             param.pMove=NA,
                             timeDep.pMove=FALSE,
                             diff.pMove=FALSE,
                             
                             nContact=n_contact_fct,
                             param.nContact=NA,
                             timeDep.nContact=FALSE,
                             diff.nContact=FALSE,
                             
                             pTrans = proba,
                             param.pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct),
                             timeDep.pTrans=FALSE,
                             diff.pTrans=FALSE,
                             
                             prefix.host="H",
                             print.progress=FALSE,
                             print.step=10)

#The simulation has run for 36 units of time and a total of 307 hosts have been infected.
#Once the simulation has finished, it reports the number of time units for which the simulation has run (36), and the maximum number of infected hosts (307). Note that the simulation has stopped here before reaching length.sim as it has crossed the max.infected threshold set at 300.




#--------Dual host
#Setting up a dual host simulation is similar to the single host version described above, but each parameter has to be provided for both hosts. Here, we choose for Host A the same parameters as the single / only host above. Host B will have sightly different parameters:

p_Exit_fctB  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16} #for a periodic function
#For pExit.B, we choose a value that depends on the “absolute” time of the simulation, for example cyclic climatic conditions (temperature). In that case, the function’s arguments should be t and prestime (the “absolute” time of the simulation), in that order
#Since pExit.B is dependent of the simulation’s time, do not forget to set timeDep.pExit.B to TRUE. Since there are no individual-based parameters nor is there influence of the host’s location, we set param.pExit.B=NA and diff.pExit.B=NA.

p_Move_fct.B  <- NA
#We will assume here that the hosts B do not move. pMove.B will then be set to NA.
#ince pMove.B is not dependent on the location, diff.pMove.B=FALSE. Similarly, there is no use of the “absolute” time of the simulation nor individual-based parameters, so param.pMove.B=NA, and timeDep.pMove.B=FALSE.

n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}
#For nContact.B, we choose a constant function that will sample a value out of a provided list of probabilities
#At each time and for each infected host, nContact.B will be drawn anew. Remember that nContact.B, like the other core functions has to be function of t, even if t is not used. Since nContact.B is constant here, there is no use of the “absolute” time of the simulation, the host’s location, nor individual-based parameters. Hence, param.nContact.B=NA, timeDep.nContact.B=FALSE and diff.nContact.B=FALSE

p_Trans_fct.B <- function(t, max.time){
  dnorm(t, mean=max.time, sd=2)*5}
#We choose pTrans.B in the form of a Gaussian function. It will reach its maximum value at a certain time point (mean) after initial infection and will subsequently decrease until it reaches 0

max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}
#Because each host is different (slightly different biotic and abiotic factors), you can expect each host to exhibit differences in the dynamics of infection, and hence the probability of transmission over time. Thus, max.time will be sampled for each host individually according to a certain distribution. max.time will be sampled from a normal distribution of parameters mean= 5 and sd= 1
    #Note again that here max.time is a function of x and not t (not a core function but              individual-based parameters), and x enters the function as the number of draws to make.

param_pTrans.B = list(max.time=max.time_fct)
#Since pTrans.B is not dependent on the “absolute” time of the simulation, timeDep.pTrans.B=FALSE. However, since we make use of individual-based parameters, we have to provide a param.pTrans as a list of functions. The name of each element of the list should have the same name as the core function (here pTrans.B) uses as argument, as shown here



#--------Running
library(nosoi)

#Transition matrix
transition.matrix <- matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),nrow = 3, ncol = 3,dimnames=list(c("A","B","C"),c("A","B","C")))

#Host A -----------------------------------

#pExit
p_Exit_fct  <- function(t,current.in){
  if(current.in=="A"){return(0.02)}
  if(current.in=="B"){return(0.05)}
  if(current.in=="C"){return(0.1)}
}

#pMove
p_Move_fct  <- function(t){return(0.1)}

#nContact
n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
proba <- function(t,p_max,t_incub){
  if(t <= t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

#Host B -----------------------------------

#pExit
p_Exit_fct.B  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16}

#pMove
p_Move_fct.B  <- NA

#nContact
n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}

#pTrans
p_Trans_fct.B <- function(t, max.time){
  dnorm(t, mean=max.time, sd=2)*5
}

max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}

param_pTrans.B = list(max.time=max.time_fct)

# Starting the simulation ------------------------------------

set.seed(60)
SimulationDual <- nosoiSim(type="dual", popStructure="discrete",
                           length.sim=300, 
                           max.infected.A=100,
                           max.infected.B=200,
                           init.individuals.A=1,
                           init.individuals.B=0,
                           init.structure.A="A",
                           init.structure.B=NA,
                           structure.matrix.A=transition.matrix,
                           structure.matrix.B=transition.matrix,
                           
                           pExit.A = p_Exit_fct,
                           param.pExit.A=NA,
                           timeDep.pExit.A=FALSE,
                           diff.pExit.A=TRUE,
                           
                           pMove.A = p_Move_fct,
                           param.pMove.A=NA,
                           timeDep.pMove.A=FALSE,
                           diff.pMove.A=FALSE,
                           
                           nContact.A=n_contact_fct,
                           param.nContact.A=NA,
                           timeDep.nContact.A=FALSE,
                           diff.nContact.A=FALSE,
                           
                           pTrans.A = proba,
                           param.pTrans.A = list(p_max=p_max_fct,t_incub=t_incub_fct),
                           timeDep.pTrans.A=FALSE,
                           diff.pTrans.A=FALSE,
                           prefix.host.A="H",
                           
                           pExit.B = p_Exit_fct.B,
                           param.pExit.B=NA,
                           timeDep.pExit.B=TRUE,
                           diff.pExit.B=FALSE,
                           
                           pMove.B = p_Move_fct.B,
                           param.pMove.B=NA,
                           timeDep.pMove.B=FALSE,
                           diff.pMove.B=FALSE,
                           
                           nContact.B=n_contact_fct.B,
                           param.nContact.B=NA,
                           timeDep.nContact.B=FALSE,
                           diff.nContact.B=FALSE,
                           
                           pTrans.B = p_Trans_fct.B,
                           param.pTrans.B = param_pTrans.B,
                           timeDep.pTrans.B=FALSE,
                           diff.pTrans.B=FALSE,
                           prefix.host.B="V",
                           
                           print.progress=FALSE)

#The simulation has run for 34 units of time and a total of 106 (A) and 129 (B) hosts have been infected.
#Once the simulation has finished, it reports the number of time units for which the simulation has run (34), and the maximum number of infected hosts A (106) and hosts B (129). Note that the simulation has stopped here before reaching length.sim as it has crossed the max.infected.A threshold set at 100.

summary(SimulationDual)

