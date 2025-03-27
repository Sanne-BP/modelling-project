rm(list=ls())
#Modify nContact function at first, have epidemiology influence contact
#--> not the other way around yet, later the population can influence epidemiology
#--> how does it influence fitness, how does that influence the probability to create offspring? 
#       o	A sicker host might have fewer offspring (lower birth rate) 
#       o	Highly transmissible virus could kill hosts faster, reducing the overall birth rate 
#       o	A host population that collapses due to infection would lead to pathogen extinction 

#Perhaps its possible to integrate some parameters for nContact
#no idea how, but lets seeeee

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







