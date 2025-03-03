#############Simulation set up
rm(list = ls())

############Spread of a pathogen in a continuous space
#Aside from the simple simulation set up, explored in another tutorial, where hosts are “not structured”, nosoi can take into account a population structured either in discrete states or in a continuous space. We focus here on a continuous space structure (for the discrete structure, see this tutorial).
#The continuous space is intended to allow the simulation to take place in a geographical context where hosts can move on a map, and may be influenced by heterogeneously distributed variables (environmental for example) on said map.
#This tutorial focuses on setting up a nosoi simulation for a pathogen which spread occurs in a continuous space

#--------Structure of the population
#continuous space
install.packages("raster")
library(raster)
set.seed(860)
test.raster <- raster(nrows=100, ncols=100, xmn=-50, xmx=50, ymn=-50,ymx=50)
test.raster[] <- runif(10000, -80, 150)
test.raster <- focal(focal(test.raster, w=matrix(1, 5, 5), mean), w=matrix(1, 5, 5), mean)
plot(test.raster)

#movement kernel
#In the case where hosts are attracted by the raster, hosts will tend to go to positions with high environmental values. Internally, once a new position is proposed, its environmental value is extracted and normalized (against the highest environmental value). This normalized number represents its probability of acceptance. Up to 30 draws can be made until one is accepted (by default the 30𝑡ℎ move is always accepted; this value was empirically set to allow enough exploration without being stuck in a time consuming search).


#--------Setting up the simulation
SimulationSingle <- nosoiSim(type="single", popStructure="continuous", ...)
#The wrapper function nosoiSim takes all the arguments that will be passed down to the simulator, in the case of this tutorial singleContinuous (for “single host, continuous space”). We thus start by providing the options type="single" and popStructure="continuous" to set up the analysis:


#--------General parameters
SimulationSingle <- nosoiSim(type="single", popStructure="continuous",
                             length.sim=300, max.infected=1000, init.individuals=1, ...)
#Here, we will run a simulation starting with 1 individual, for a maximum of 1,000 infected individuals and a maximum time of 300 days.
    #init.individuals defines the number of individuals (an integer above 1) that will start a         transmission chain. Keep in mind that you will have as many transmission chains as initial         individuals, which is equivalent as launching a number of independent nosoi simulations.
    #init.structure specifies the original location (as a vector of two coordinates, x and y) in       the continuous space (has to be the same starting location for all starting individuals). The      location provided in init.structure should of course be present in the structure.raster.


#--------Dual host
SimulationDual <- nosoiSim(type="dual", popStructure="continuous", ...)
#In the case of a dual host simulation, several parameters of the nosoiSim will have to be specified for each host type, designated by A and B. The wrapper function nosoiSim will then take all the arguments that will be passed down to the simulator, in the case of this tutorial dualContinuous (for “dual host, continuous structure”). We thus start by providing the options type="dual" and popStructure="continuous" to set up the analysis


##############RUNNING NOSOI
#------Single host
#We present here a very simple simulation for a single host pathogen

p_Exit_fct  <- function(t, current.env.value){
  if(current.env.value > 60){p=0.02}
  if(current.env.value < 60 && current.env.value > 30){p=0.04}
  if(current.env.value < 30){p=0.08}
  return(p)}
#We choose a probability that only depends on the environmental value of the location where the host currently is. The maximum value of our environmental raster is close to 70, and the higher the environmental value is, the less likely the host is to die.
#Remember that pExit, like the other core functions has to be function of t, even if t is not used. Since pExit is dependent on the location, diff.pMove=TRUE. However, there is no use of the “absolute” time of the simulation nor individual-based parameters, so param.pExit=NA, and timeDep.pExit=FALSE.

p_Move_fct  <- function(t){return(0.1)}
#constant value, namely 0.1 which means that an infected host has 10% chance to change its location for each unit of time. 
#Remember that pMove, like the other core functions has to be function of t, even if t is not used. Since pMove is not dependent on the location, diff.pMove=FALSE. Similarly, there is no use of the “absolute” time of the simulation nor individual-based parameters, so param.pMove=NA, and timeDep.pMove=FALSE

n_contact_fct <- function(t){abs(round(rnorm(1, 0.5, 1), 0))}
#constant function that will draw a value in a normal distribution of mean=0.5 and sd=1, round it, and take its absolute value
#At each time and for each infected host, nContact will be drawn anew. Remember that nContact, like the other core functions has to be function of t, even if t is not used. Since nContact is constant here, there is no use of the “absolute” time of the simulation, the location of the host, nor individual-based parameters. So param.nContact=NA, timeDep.nContact=FALSE and diff.nContact=FALSE

p_Trans_fct <- function(t, p_max, t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)}
#We choose pTrans in the form of a threshold function: before a certain amount of time since initial infection, the host does not transmit (incubation time, which we call t_incub), and after that time, it will transmit with a certain (constant) probability (which we call p_max). This function is dependent of the time since the host’s infection t

t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
#Because each host is different (slightly different biotic and abiotic factors), you can expect each host to exhibit differences in the dynamics of infection, and hence the probability of transmission over time. Thus, t_incub and p_max will be sampled for each host individually according to a certain distribution. t_incub will be sampled from a normal distribution of mean= 7 and sd=1, while p_max will be sampled from a beta distribution with shape parameters𝛼=5 and𝛽=2
#Note that here t_incub and p_max are functions of x and not t (they are not core functions but individual-based parameters), and x enters the function as the number of draws to make.

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)
#pTrans is not dependent on the “absolute” time of the simulation, nor on the hosts location, so timeDep.pTrans=FALSE and diff.pTrans=FALSE. However, since we make use of individual-based parameters, we have to provide a param.pTrans as a list of functions. The name of each element within this list should have the same name that the core function (here pTrans) uses as argument





#------------------------------Running Single Host------------------------------------------------
library(nosoi)

#Raster is test.raster

#Starting position will be
start.pos <- c(0,0) # c(x,y)

#pExit
p_Exit_fct  <- function(t, current.env.value){
  if(current.env.value > 60){p=0.02}
  if(current.env.value < 60 && current.env.value > 30){p=0.04}
  if(current.env.value < 30){p=0.08}
  return(p)}

#pMove
p_Move_fct  <- function(t){return(0.1)}

#sdMove
sd_Move_fct  <- function(t){return(0.25)}

#nContact
n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
proba <- function(t,p_max,t_incub){
  if(t <= t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)}

t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

# Starting the simulation ------------------------------------
set.seed(846)
SimulationSingle <- nosoiSim(type="single", popStructure="continuous",
                             length.sim=300, max.infected=300, init.individuals=1, 
                             
                             init.structure=start.pos, 
                             
                             structure.raster=test.raster,
                             
                             pExit = p_Exit_fct,
                             param.pExit = NA,
                             timeDep.pExit=FALSE,
                             diff.pExit=TRUE,
                             
                             pMove = p_Move_fct,
                             param.pMove = NA,
                             timeDep.pMove=FALSE,
                             diff.pMove=FALSE,
                             
                             sdMove = sd_Move_fct,
                             param.sdMove = NA,
                             timeDep.sdMove=FALSE,
                             diff.sdMove=FALSE,
                             
                             attracted.by.raster=FALSE,
                             
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

#Once the simulation has finished, it reports the number of time units for which the simulation has run (40), and the maximum number of infected hosts (350). Note that the simulation has stopped here before reaching length.sim as it has crossed the max.infected threshold set at 300.






#------------------------------Running Dual Host--------------------------------------------------
#Setting up a dual host simulation is similar to the single host version described above, but each parameter has to be provided for both hosts. Here, we choose for Host A the same parameters as the single / only host above. Host B will have sightly different parameters:

p_Exit_fctB  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16} #for a periodic function
#a value that depends on the “absolute” time of the simulation, for example cyclic climatic conditions (temperature). In that case, the function’s arguments should be t and prestime (the “absolute” time of the simulation), in that order
#Since pExit.B is dependent on the simulation’s time, do not forget to set timeDep.pExit.B to TRUE Since there are no individual-based parameters nor is there any influence of the host’s location, param.pExit.B=NA and diff.pExit.B=NA

p_Move_fct.B  <- NA
#We assume here that the hosts B do not move. pMove.B will hence be set to NA
#Since pMove.B is not dependent on the location, diff.pMove.B=FALSE. Similarly, there is no use of the “absolute” time of the simulation nor individual-based parameters, so param.pMove.B=NA, and timeDep.pMove.B=FALSE

sd_Move_fct.B  <- NA
#Since hosts B do not move, sdMove.B is irrelevant and will be set to NA
#Since sdMove.B is not dependent on the location, diff.sdMove.B=FALSE. Similarly, there is no use of the “absolute” time of the simulation nor individual-based parameters, so param.sdMove.B=NA, and timeDep.sdMove.B=FALSE

n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}
#a constant function that will sample a value from a provided list of values, with a certain probability
#At each time and for each infected host, nContact.B will be drawn anew. Remember that nContact.B, like the other core functions has to be function of t, even if t is not used. Since nContact.B is constant here, there is no use of the “absolute” time of the simulation, the host’s location, nor individual-based parameters. So param.nContact.B=NA, timeDep.nContact.B=FALSE and diff.nContact.B=FALSE

p_Trans_fct.B <- function(t, max.time){
  dnorm(t, mean=max.time, sd=2)*5}
#We choose pTrans.B in the form of a Gaussian function. It will reach its maximum value at a certain time point (mean) after initial infection and will subsequently decrease until it reaches 0. The function dnorm used here will achieve this objective: for each time t after infection, its return value will reach its maximum value (< 1) at its mean (here max.time, a parameter that will be set for each individual, see below) and then decline back to 0. Its increase and decline is parameterized by the standard deviation sd of the dnorm function.

max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}
#because each host is different (slightly different biotic and abiotic factors), you can expect each host to exhibit differences in the dynamics of infection, and hence the probability of transmission over time. Thus, max.time will be sampled for each host individually according to a certain distribution. max.time will be sampled from a normal distribution of parameters mean= 5 and sd= 1
#Note again that here max.time is a function of x and not t (i.e. not a core function but individual-based parameters), and x enters the function as the number of draws to make.

param_pTrans.B = list(max.time=max.time_fct)
#Since pTrans.B is not dependent of the “absolute” time of the simulation nor the host’s location, timeDep.pTrans.B=FALSE and diff.pTrans.B=FALSE. However, since we have made use of individual-based parameters, we have to provide a param.pTrans as a list of functions. The names of each element of the list should have the same name that the core function (pTrans.B here) uses as its argument


#----------------------Running
library(nosoi)

#Raster is test.raster

#Starting position will be
start.pos <- c(0,0) # c(x,y)

#Host A -----------------------------------

#pExit
p_Exit_fct  <- function(t, current.env.value){
  if(current.env.value > 60){p=0.02}
  if(current.env.value < 60 && current.env.value > 30){p=0.04}
  if(current.env.value < 30){p=0.08}
  return(p)}

#pMove
p_Move_fct  <- function(t){return(0.1)}

#sdMove
sd_Move_fct  <- function(t){return(0.25)}

#nContact
n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
proba <- function(t,p_max,t_incub){
  if(t <= t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)}

t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

#Host B -----------------------------------

#pExit
p_Exit_fct.B  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16}

#pMove
p_Move_fct.B  <- NA

#pMove
sd_Move_fct.B  <- NA

#nContact
n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}

#pTrans
p_Trans_fct.B <- function(t, max.time){
  dnorm(t, mean=max.time, sd=2)*5}

max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}

param_pTrans.B = list(max.time=max.time_fct)

# Starting the simulation ------------------------------------
set.seed(60)
SimulationDual <- nosoiSim(type="dual", popStructure="continuous",
                           length.sim=300, 
                           max.infected.A=100,
                           max.infected.B=200,
                           init.individuals.A=1,
                           init.individuals.B=0,
                           init.structure.A=start.pos,
                           init.structure.B=NA,
                           structure.raster.A=test.raster,
                           structure.raster.B=test.raster,
                           
                           pExit.A = p_Exit_fct,
                           param.pExit.A = NA,
                           timeDep.pExit.A=FALSE,
                           diff.pExit.A=TRUE,
                           
                           pMove.A = p_Move_fct,
                           param.pMove.A = NA,
                           timeDep.pMove.A=FALSE,
                           diff.pMove.A=FALSE,
                           
                           sdMove.A = sd_Move_fct,
                           param.sdMove.A = NA,
                           timeDep.sdMove.A=FALSE,
                           diff.sdMove.A=FALSE,
                           attracted.by.raster.A=FALSE,
                           
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
                           param.pExit.B = NA,
                           timeDep.pExit.B=TRUE,
                           diff.pExit.B=FALSE,
                           
                           pMove.B = p_Move_fct.B,
                           param.pMove.B = NA,
                           timeDep.pMove.B=FALSE,
                           diff.pMove.B=FALSE,
                           
                           sdMove.B = sd_Move_fct.B,
                           param.sdMove.B = NA,
                           timeDep.sdMove.B=FALSE,
                           diff.sdMove.B=FALSE,
                           attracted.by.raster.B=FALSE,
                           
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

#Once the simulation has finished, it reports the number of time units for which the simulation has run (33), and the maximum number of infected hosts A (102) and hosts B (137). Note that the simulation has stopped here before reaching length.sim as it has crossed the max.infected.A threshold set at 100








