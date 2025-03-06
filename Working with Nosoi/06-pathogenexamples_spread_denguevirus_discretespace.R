#################Pathogen examples: Spread of dengue virus in a discrete space#####################
rm(list = ls())
gc()  # Clean up memory in R

#-------------Introduction
#In this example, our objective is to simulate a dengue-like epidemic in South-East Asia. Dengue is a dual-host pathogen (arbovirus) that spreads between mosquitoes and humans, with very different within-host dynamics. We will also take into account the location’s influence on mosquito activity by using the local monthly average temperature: the higher the temperature, the more mosquito activity (i.e. the more mosquitoes will bite a human at each time step). Mosquitoes will not move between countries, but humans will; we will use air transportation data to approximate the transition probability matrix between the different countries.

#Most of the host parameters used for the simulation will be taken from the following published article: Fontaine et al., (2018)


#-------------Study Site
#Study will focus on 5 south-east Asian countries: China (only the southern part of the country), Cambodia, Laos, Thailand and Vietnam
#3 letter code of countries of interest:
SE.Asia <- c("CHN","KHM","LAO","THA","VNM")
study.area.SE.Asia <- extent(90, 125, 5, 30) #to crop the raster to the study site


#-------------Average annual temperature
#Here we use WorldClim v2 data (average temperature (°C) - tavg 10m). The rasters are loaded, cut to fit the study site, and the temperature value averaged for all countries (i.e. states in our simulation) each month. Country spatial polygons come from GADM.

#SCRIPT FROM THE GITHUB SITE DOESN'T WORK ANYMORE AS library(rgdal) IS OUTDATED
#--> update your code to replace rgdal and rgeos with sf and terra. (WITH CHATGPT)

# Load required packages
library(sf)      # For spatial vector data
library(terra)   # For raster processing
library(dplyr)   # For data manipulation

#loading the data from github 
avgTemp_data <- read_csv("data/avgTemp_data.csv")

# Check the data structure
head(avgTemp_data)

#We can visualize these data on a map, 
#This is the pattern of average temperature for each country over the year::
ggplot(avgTemp_data, aes(x = Month, y = Temp, color = country)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Temperature Trends by Country",
       x = "Month", y = "Average Temperature (°C)",
       color="Country") +
  theme_minimal()

#Using these data, we will fit a trigonometric function to each of the locations to obtain a function that links day of the year to mean temperature. The function will be of the form 𝑇𝑎𝑣𝑔=𝑘1×𝑠𝑖𝑛(2𝜋365𝑡)+𝑘2×𝑐𝑜𝑠(2𝜋365𝑡)+𝛽(Eq. 1). We fit this function on data from each location to get the values of 𝑘1, 𝑘2and 𝛽:
model.coef <- NULL
for(current.country in SE.Asia){
  final.results.country <- subset(final.results, country == current.country)
  
  reslm <- lm(final.results.country$Temp ~ sin(2*pi/365*final.results.country$Day)+cos(2*pi/365*final.results.country$Day))
  
  model.coef <- rbind.data.frame(model.coef,
                                 data.frame(country=current.country, 
                                            Beta=reslm$coefficients[1], 
                                            k1=reslm$coefficients[2], 
                                            k2=reslm$coefficients[3]))}
  
#het werkt allemaal niet, omdat ik die data niet kan laden.
