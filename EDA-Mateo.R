# Exploratory data analysis : nature of covariates and distribution of response, presence of outliers
# or missing values, range of variables.

source("main.R") # get the function plot.Country
library(gam) # not default lib in my current version of R
library(ggplot2)
library(dyn)

DeathsByCountry <- unlist(readRDS("DeathsByCountry.rds")[[1]])
CasesByCountry <- unlist(readRDS("CasesByCountry.rds")[[1]])
CountryPop <- unlist(readRDS("CountryPop.rds")[[1]])
CountryNames <- readRDS("CountryNames.rds")

# Explore some patterns in Asian, in specific in Kuwait,Saudi_Arabia and United_Arab_Emirates

#Europe
par(mar=c(1,1,1,1))
deaths.cases.Continent("Europe")

par(mfrow=c(1,3))
plot.Country("Kuwait",names=CountryNames,  deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop,plot=T,plot.cumul = T,xmin=50)
plot.Country("Saudi_Arabia",names=CountryNames,  deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop, plot=T,plot.cumul=T,xmin=50)
plot.Country("United_Arab_Emirates", names=CountryNames, deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop, plot=T,plot.cumul=T,xmin=50)
#plots Asia
plot.all.countries.Continent("Asia")
dist.deaths.cases.Continent("Asia")
###UAE
kuw_d <- DeathsByCountry["Kuwait",]
kuw_c<- CasesByCountry["Kuwait",]
par(mfrow=c(1,2))
hist(kuw_c,main = "Kuwait cases density",breaks=15,xlab = "New cases per day")
abline(v = mean(kuw_c), col = "blue", lwd = 2)
hist(kuw_d,main="Kuwait deaths densitiy",breaks = 15,xlab="Deaths per day")
abline(v = mean(kuw_d), col = "blue", lwd = 2)

par(mfrow=c(1,2))
boxplot(kuw_c,main= "Kuwait",ylab="Cases per day")
boxplot(kuw_d,main= "Kuwait",ylab="Death per day")
hist(log(1+kuw_c),main = "Kuwait log cases density",breaks=15,xlab = "New cases per day(log)")
abline(v = mean(log(1+kuw_c)), col = "blue", lwd = 2)
hist(log(1+kuw_d),main="Kuwait log deaths densitiy",breaks = 15,xlab="Deaths per day(log)")
abline(v = mean(log(1+kuw_d)), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(log(1+kuw_c),main= "Kuwait",ylab="Cases per day(log)")
boxplot(log(1+kuw_d),main= "Kuwait",ylab="Death per day(log)")
###Outliers, missing values
tail(sort(kuw_d))#8,9,9,9,9,10
tail(sort(kuw_c))#958,965,973,975,977,987
#range of variables
range(kuw_d) #range of deaths per day 0-10
range(kuw_c) #range of cases per day 0-987
###Saudi_Arabia
sau_d <- DeathsByCountry["Saudi_Arabia",]
sau_c<- CasesByCountry["Saudi_Arabia",]
par(mfrow=c(1,2))
hist(sau_c,main = "Saudi_Arabia cases density",breaks=15,xlab = "New cases per day")
abline(v = mean(sau_c), col = "blue", lwd = 2)
hist(sau_d,main="Saudi_Arabia deaths densitiy",breaks = 15,xlab="Deaths per day")
abline(v = mean(sau_d), col = "blue", lwd = 2)

par(mfrow=c(1,2))
boxplot(uae_c,main= "Saudi_Arabia",ylab="Cases per day")
boxplot(uae_d,main= "Saudi_Arabia",ylab="Death per day")
hist(log(1+sau_c),main = "Saudi_Arabia log cases density",breaks=15,xlab = "New cases per day(log)")
abline(v = mean(log(1+sau_c)), col = "blue", lwd = 2)
hist(log(1+sau_d),main="Saudi_Arabia log deaths densitiy",breaks = 15,xlab="Deaths per day(log)")
abline(v = mean(log(1+sau_d)), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(log(1+sau_c),main= "Saudi_Arabia",ylab="Cases per day(log)")
boxplot(log(1+sau_d),main= "Saudi_Arabia",ylab="Death per day(log)")
###Outliers, missing values
tail(sort(sau_d))#13,14,16,18,21,23
tail(sort(sau_c))#2577,2598,2613,2628,2723
par(mfrow=c(1,3))
###range of variables
range(sau_d) #range of deaths per day 0-23
range(sau_c) #range of cases per day 0-2723
###UAE
uae_d <- DeathsByCountry["United_Arab_Emirates",]
uae_c<- CasesByCountry["United_Arab_Emirates",]
par(mfrow=c(1,2))
hist(uae_c,main = "UAE cases density",breaks=15,xlab = "New cases per day")
abline(v = mean(uae_c), col = "blue", lwd = 2)
hist(uae_d,main="UAE deaths densitiy",breaks = 15,xlab="Deaths per day")
abline(v = mean(uae_d), col = "blue", lwd = 2)

par(mfrow=c(1,2))
boxplot(uae_c,main= "UAE",ylab="Cases per day")
boxplot(uae_d,main= "UAE",ylab="Death per day")
hist(log(1+uae_c),main = "UAE log cases density",breaks=15,xlab = "New cases per day(log)")
abline(v = mean(log(1+uae_c)), col = "blue", lwd = 2)
hist(log(1+uae_d),main="UAE log deaths densitiy",breaks = 15,xlab="Deaths per day(log)")
abline(v = mean(log(1+uae_d)), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(log(1+uae_c),main= "UAE",ylab="Cases per day(log)")
boxplot(log(1+uae_d),main= "UAE",ylab="Death per day(log)")
###Outliers, missing values
tail(sort(uae_d))#9,9,9,9,10,11
tail(sort(uae_c))#828,862,882,900,903,943
par(mfrow=c(1,3))
plot.Country("United_Arab_Emirates",names=CountryNames,  deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop,plot=T,plot.cumul = T,xmin=50)
###range of variables
range(uae_d) #range of deaths per day 0-11
range(uae_c) #range of cases per day 0-943


