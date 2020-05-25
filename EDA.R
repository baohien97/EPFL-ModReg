# Exploratory data analysis : nature of covariates and distribution of response, presence of outliers
# or missing values, range of variables.

source("main.R") # get the function plot.Country
library(gam) # not default lib in my current version of R

DeathsByCountry <- read.csv("DeathsByCountry.csv")
CasesByCountry <- read.csv("CasesByCountry.csv")
CountryPop <- read.csv("CountryPop.csv")
CountryNames <- read.csv("CountryNames.csv")
  
plot.Country("Switzerland", names=CountryNames, deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop, plot=T)