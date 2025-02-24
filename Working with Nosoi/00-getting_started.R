#Getting started with Nosoi (https://slequime.github.io/nosoi/index.html)
rm(list = ls())

#Installation
install.packages("nosoi")
library(nosoi)

#Setting up the core functions
p_Function  <- function(t){0.08}
            #nosoi expects time-dependent functions, so a function must include t in its definition
p_Function  <- function(t){plogis(t,10,2)}
            #plogis(t, 10, 2) represents a logistic growth function. this means the probability                starts small and gradually icreases, approaching 1 over time. Parameters (10,2) 10 >               midpoint where probability is 50%, 2 > controls how fast probability increases. 
            #If this function controls probability of exiting infection, an infected host would                have a low chance of exiting early but a higher chance of exiting after 10 time steps

p_Function  <- function(t,prestime){(sin(prestime/12)+1)/2}
            #prestime = absolute simulation time (same for all hosts), it is global and represents             the overall time progression of the entire epidemic simulation. Useful when modeling               factors that affects all hosts simultaneously, like seasonality or periodic                        environmental changes.
            #sinus naturally oscillates between -1 and 1, therefore you need to transform it so                that the range becomes 0 to 1 instead. This is done by adding 1 (moves range from [-1              ,1] to [0,2] and dividing by 2 (adjusts range from [0,2] to [0,1].
            #by dividing by 12, the wave is stretched so that it completes one full cycle over 12              time units > 12 represents natural time cycle (1 year)








