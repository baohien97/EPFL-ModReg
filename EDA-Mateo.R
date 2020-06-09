# Exploratory data analysis : nature of covariates and distribution of response, presence of outliers
# or missing values, range of variables.

source("main.R") # get the function plot.Country
library(gam) # not default lib in my current version of R
library(ggplot2)

DeathsByCountry <- unlist(readRDS("DeathsByCountry.rds")[[1]])
CasesByCountry <- unlist(readRDS("CasesByCountry.rds")[[1]])
CountryPop <- unlist(readRDS("CountryPop.rds")[[1]])
CountryNames <- readRDS("CountryNames.rds")

# Explore some patterns in Europe, Oceania, other and some countries in these three continents

#Europe
par(mar=c(1,1,1,1))
deaths.cases.Continent("Europe")


par(mfrow=c(1,3))
plot.Country("Switzerland",names=CountryNames,  deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop,plot=T,plot.cumul = T,xmin=50)
plot.Country("Netherlands",names=CountryNames,  deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop, plot=T,plot.cumul=T,xmin=50)
plot.Country("United_Kingdom", names=CountryNames, deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop, plot=T,plot.cumul=T,xmin=50)
####Switzerland
sw_d <- DeathsByCountry["Switzerland",]
sw_c<- CasesByCountry["Switzerland",]
par(mfrow=c(1,2))
hist(sw_c,main = "Switzerland cases density",breaks=15,xlab = "New cases per day")
abline(v = mean(sw_c), col = "blue", lwd = 2)
hist(sw_d,main="Switzerland deaths densitiy",breaks = 15,xlab="Deaths per day")
abline(v = mean(sw_d), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(sw_c,main= "Switzerland",ylab="Cases per day")
boxplot(sw_d,main= "Switzerland",ylab="Death per day")
par(mfrow=c(1,2))
hist(log(1+sw_c),main = "Switzerland cases density",breaks=15,xlab = "New cases per day(log)")
abline(v = mean(log(1+sw_c)), col = "blue", lwd = 2)
hist(log(1+sw_d),main="Switzerland deaths densitiy",breaks = 15,xlab="Deaths per day(log)")
abline(v = mean(log(1+sw_d)), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(log(1+sw_c),main= "Switzerland",ylab="Cases per day(log)")
boxplot(log(1+sw_d),main= "Switzerland",ylab="Death per day(log)")
###Outliers
tail(sort(sw_d))##49,53,53,54,55,57
tail(sort(sw_c))##1044,1058,1103,1105,1146,1187
####Netherlands
ned_d <- DeathsByCountry["Netherlands",]
ned_c<- CasesByCountry["Netherlands",]
par(mfrow=c(1,2))
hist(ned_c,main = "Netherlands cases density",breaks=15,xlab = "New cases per day")
abline(v = mean(ned_c), col = "blue", lwd = 2)
hist(ned_d,main="Netherlands deaths densitiy",breaks = 15,xlab="Deaths per day")
abline(v = mean(ned_d), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(ned_c,main= "Netherlands",ylab="Cases per day")
boxplot(ned_d,main= "Netherlands",ylab="Death per day")
par(mfrow=c(1,2))
hist(log(1+ned_c),main = "Netherlands cases density",breaks=15,xlab = "New cases per day(log)")
abline(v = mean(log(1+ned_c)), col = "blue", lwd = 2)
hist(log(1+ned_d),main="Netherlands deaths densitiy",breaks = 15,xlab="Deaths per day(log)")
abline(v = mean(log(1+ned_d)), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(log(1+ned_c),main= "Netherlands",ylab="Cases per day(log)")
boxplot(log(1+ned_d),main= "Netherlands",ylab="Death per day(log)")
####Outlier
tail(sort(ned_d))#158,159,161,164,171,176
tail(sort(ned_c))#1145,1147,1151,1172,1275,1288
####United_Kingdom
uk_d <- DeathsByCountry["United_Kingdom",]
uk_c<- CasesByCountry["United_Kingdom",]
par(mfrow=c(1,2))
hist(uk_c,main = "United_Kingdom cases density",breaks=15,xlab = "New cases per day")
abline(v = mean(uk_c), col = "blue", lwd = 2)
hist(uk_d,main="United_Kingdom deaths densitiy",breaks = 15,xlab="Deaths per day")
abline(v = mean(uk_d), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(uk_c,main= "United_Kingdom",ylab="Cases per day")
boxplot(uk_d,main= "United_Kingdom",ylab="Death per day")
hist(log(1+uk_c),main = "United_Kingdom cases density",breaks=15,xlab = "New cases per day(log)")
abline(v = mean(log(1+uk_c)), col = "blue", lwd = 2)
hist(log(1+uk_d),main="United_Kingdom deaths densitiy",breaks = 15,xlab="Deaths per day(log)")
abline(v = mean(log(1+uk_d)), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(log(1+uk_c),main= "United_Kingdom",ylab="Cases per day(log)")
boxplot(log(1+uk_d),main= "United_Kingdom",ylab="Death per day(log)")
###Outliers
tail(sort(uk_d))#935,972,1026,1031,1058,1096
tail(sort(uk_c))#5458,5658,6086,6116,6401

#Oceania
par(mar=c(1,1,1,1))
deaths.cases.Continent("Oceania",c("Australia","New_Zealand"))
par(mfrow=c(1,3))
plot.Country("Australia",names=CountryNames,  deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop,plot=T,plot.cumul = T,xmin=50)
plot.Country("New_Zealand",names=CountryNames,  deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop,plot=T,plot.cumul = T,xmin=50)
#plot.Country("Papua_New_Guinea",names=CountryNames,  deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop,plot=T,plot.cumul = T,xmin=50)
###Australia
au_d <- DeathsByCountry["Australia",]
au_c<- CasesByCountry["Australia",]
par(mfrow=c(1,2))
hist(au_c,main = "Australia cases density",breaks=15,xlab = "New cases per day")
abline(v = mean(au_c), col = "blue", lwd = 2)
hist(au_d,main="Australia deaths densitiy",breaks = 15,xlab="Deaths per day")
abline(v = mean(au_d), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(au_c,main= "Australia",ylab="Cases per day")
boxplot(au_d,main= "Australia",ylab="Death per day")
hist(log(1+au_c),main = "Australia cases density",breaks=15,xlab = "New cases per day(log)")
abline(v = mean(log(1+au_c)), col = "blue", lwd = 2)
hist(log(1+au_d),main="Australia deaths densitiy",breaks = 15,xlab="Deaths per day(log)")
abline(v = mean(log(1+au_d)), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(log(1+au_c),main= "Australia",ylab="Cases per day(log)")
boxplot(log(1+au_d),main= "Australia",ylab="Death per day(log)")
###Outliers
tail(sort(au_d))#3,4,4,4,4,5
tail(sort(au_c))#333,337,363,393,442,448
###New_Zealand
nz_d <- DeathsByCountry["New_Zealand",]
nz_c<- CasesByCountry["New_Zealand",]
par(mfrow=c(1,2))
hist(nz_c,main = "New_Zealand cases density",breaks=15,xlab = "New cases per day")
abline(v = mean(nz_c), col = "blue", lwd = 2)
hist(nz_d,main="New_Zealand deaths densitiy",breaks = 15,xlab="Deaths per day")
abline(v = mean(nz_d), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(nz_c,main= "New_Zealand",ylab="Cases per day")
boxplot(nz_d,main= "New_Zealand",ylab="Death per day")
hist(log(1+nz_c),main = "New_Zealand cases density",breaks=15,xlab = "New cases per day(log)")
abline(v = mean(log(1+nz_c)), col = "blue", lwd = 2)
hist(log(1+nz_d),main="New_Zealand deaths densitiy",breaks = 15,xlab="Deaths per day(log)")
abline(v = mean(log(1+nz_d)), col = "blue", lwd = 2)
par(mfrow=c(1,2))
boxplot(log(1+nz_c),main= "New_Zealand",ylab="Cases per day(log)")
boxplot(log(1+nz_d),main= "New_Zealand",ylab="Death per day(log)")
###Outliers
tail(sort(nz_d))#1,1,1,1,2,2
tail(sort(nz_c))#59,65,71,71,76,77
#Other
plot.Country("Cases_on_an_international_conveyance_Japan",names=CountryNames,  deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop,plot=T,plot.cumul = T,xmin=50)





