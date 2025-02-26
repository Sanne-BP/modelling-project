######Getting started with Nosoi (https://slequime.github.io/nosoi/index.html)
rm(list = ls())

#Installation from CRAN:
#install.packages("nosoi")
#library(nosoi)

#Installing latest version from github (preferred):
install.packages("devtools")
devtools::install_github(repo = "slequime/nosoi")
      #I did not update the packages yet, but by reinstalling nosoi this is probably possible again

#######Setting up the core functions
###Functions' arguments

#t = time
p_Function  <- function(t){0.08}
            #nosoi expects time-dependent functions, so a function must include t in its definition
p_Function  <- function(t){plogis(t,10,2)}
            #plogis(t, 10, 2) represents a logistic growth function. this means the probability                starts small and gradually icreases, approaching 1 over time. Parameters (10,2) 10 >               midpoint where probability is 50%, 2 > controls how fast probability increases. 
            #If this function controls probability of exiting infection, an infected host would                have a low chance of exiting early but a higher chance of exiting after 10 time steps

#prestime
p_Function  <- function(t,prestime){(sin(prestime/12)+1)/2}
            #prestime = absolute simulation time (same for all hosts), it is global and represents             the overall time progression of the entire epidemic simulation. Useful when modeling               factors that affects all hosts simultaneously, like seasonality or periodic                        environmental changes.
            #sinus naturally oscillates between -1 and 1, therefore you need to transform it so                that the range becomes 0 to 1 instead. This is done by adding 1 (moves range from [-1              ,1] to [0,2] and dividing by 2 (adjusts range from [0,2] to [0,1].
            #by dividing by 12, the wave is stretched so that it completes one full cycle over 12              time units > 12 represents natural time cycle (1 year)

#current.in and current.env.value
p_Function  <- function(t,current.in){
  if(current.in=="A"){return(0)} #if in A, probability is 0 etc.
  if(current.in=="B"){return(0.5)}
  if(current.in=="C"){return(1)}} #discrete (between states "A","B" and "C")
            #current.in = discrete structure, when locations are categorical (A, B, C etc.).                   Function assigns different probabilities based on the current location
                #can be used to represent the location of the host (only if a structured population                 is used, simulations where the location of hosts affect their probability of event,                 like transmission or movement)
  
p_Function  <- function(t,current.env.value){current.env.value/100} #continuous
            #current.env.value = continuous structure, function assigns probabilities based on a                numerical value
            #if environmental value is 50, probability is 50/100=0.5, could model temperature                  -dependent transmission, where a pathogen is more transmissible in hotter areas 

          #discrete = transmission probably depends on categorical location
          #continuous = transmission probability depends on a numeric environmental variable, or             movement 

#host.count (can be used to represent the number of hosts present at a given location (only for a structured population))
p_Function  <- function(t,current.in, host.count){
  if(current.in=="A"){return(0)}
  if(current.in=="B" & host.count < 300 ){return(0.5)}
  if(current.in=="B" & host.count >= 300 ){return(0)}
  if(current.in=="C"){return(1)}} #discrete (between states "A","B" and "C")
          #allows simulation to model density-dependent effects, meaning that probabilities (e.g.            of transmission, movement, or exiting) can change based on how crowded a location is 
          #If the host is in location A, probability is 0. If host is in B and number of hosts is            below 300, the probability is 0.5 etc.
          #transmission depends on location and host density

p_Function  <- function(t,current.env.value,host.count){(current.env.value-host.count)/100} #continuous
          #function calculates probability based on continuous environmental factor and the number           of hosts. probability decreases as host.count increases (because host.count is                     subtracted). Could model resource competition, e.g. limited food or medical resources              reduce transmission potential in highly populated areas.
          #transmission depends on environmental factors and host density
    
###Extra Parameters
#You can introduce heterogeneity among hosts by making certain parameters individual-specific rather than global constants. This allows each host to have unique characteristics that affect their behavior in the epidemic model. 

p_Function  <- function(t,pFunction.param1){plogis(t,pFunction.param1,2)}
          #p_Function  <- function(t){plogis(t,10,2)} <- it was first this!
          #the function is defined to depend on t and an additional parameter, pFunction.param1
          #plogis(t,pFunction.param1,2) defines a logistic function, where pFunction.param1                  represents the mean µ of the distribution. SO, instead of setting µ=10 as a fixed value            for all hosts, we now make it vary between individuals

    #To introduce variability among hosts, pFunction.param1 is sampled from a normal distribution      with mean = 10 and sd = 2
    p_Function_param1 <- function(x){rnorm(x,mean=10,sd=2)} #sampling one parameter for each                                                                   infected individual
          #this generates one random value per host, meaning each infected host gets a unique                 pFunction.param1 value. Notice that the function is expressed as a function of x instead           of t. x is present in the body of the function as the number of draws to make from the             distribution

    p_Function_parameters  <- list(pFunction.param1 = p_Function_param1)
          #every parameter function you specify should be gathered into a list, where the function           determining the parameter for each individual (here, p_Function_param1) has to be named            according to the name used in p_Function for this parameter (here, pFunction.param1) 
          #The list acts as a structured "parameter manager", allowing nosoi to efficiently assign           individualized parameter values to each host without needing to manually specify them              every time.


###Combining arguments
p_Function  <- function(t, prestime, current.in, pFunction.param1, pFunction.param2,...){}   

  #We have previously shown that you can combine the time since infection t with other parameters    such as current.in or prestime. In fact, you can combine as many arguments as you want, making a   function dependent on the time since infection, current location, present time and individual      host-dependent parameters. They however need to respect a specific order to be correctly parsed    by the   simulator: first t, then prestime, then current.in (discrete) or current.env.value        (continuous) and finally individual host-dependent parameters.
