#############Simulation set up
rm(list = ls())

#########Spread of a pathogen in a homogeneous population
#nosoi can accommodate a wide range of epidemiological transmission scenarios. It hence relies on many parameters, that need to be set properly for the right scenario to be simulated. This tutorial aims to illustrate how to set up a nosoi simulation for a “simple” case: a pathogen being transmitted within a population without structure. We will present two cases, first for a single-host, and then a dual-host pathogen.

######Setting up the simulation
#Setting up a basic pathogen spread simulation in a homogeneous population with no structured environment. Function nosoiSim is the main wrapper for running simulations in nosoi, in this case we use singleNone "single host, no structure" setup. So we start by providing the options type="single" and popStructure="none". 
SimulationSingle <- nosoiSim(type="single", popStructure="none", ...)

