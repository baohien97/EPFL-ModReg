source("main.R") # get the function plot.Country
library(gam) # not default lib in my current version of R
library(ggplot2)
library(mgcv)
library(aods3)
library(MASS)
library(EnvStats)
deaths <- read.csv("latest_data.csv")
DeathsByCountry <- unlist(readRDS("DeathsByCountry.rds")[[1]])
CasesByCountry <- unlist(readRDS("CasesByCountry.rds")[[1]])
CountryPop <- unlist(readRDS("CountryPop.rds")[[1]])
CountryNames <- readRDS("CountryNames.rds")

###modelling,glm,gam

# We model number of deaths as a fraction of cases confirmed on l previous 
# days (smoothened)

l_max = 10 # number of previous days to take into account
par(mfrow=c(1,3))
plot.Country("Kuwait", names=CountryNames, deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop, plot=T,plot.cumul=T,xmin=50)
# Kuwait, Saudi Arabia and United Arab Emirates
country = "Kuwait"
deaths.country = DeathsByCountry[country,]
cases.country = CasesByCountry[country,]
mu.deaths.t = c()
mu.cases.t = c()

for (i in (l_max + 1):length(deaths.country)){
  mu.cases.t[i-l_max] <- log(1+mean(cases.country[(i - l_max): i]))  # plus 1 to avoid log(0)
  mu.deaths.t[i-l_max] <-log(1+deaths.country[i])
}

merged = data.frame(mu.deaths.t, mu.cases.t)
###linear
lin.mod <- glm(mu.deaths.t ~ mu.cases.t, data=merged)
summary(lin.mod)
par(mfrow=c(1,4)) # Change the panel layout to 1 x 4
plot(lin.mod)
###Poisson
#poiss.mod <- glm(mu.deaths.t ~ mu.cases.t,family=poisson(link="log"), data=merged)
#summary(poiss.mod)
#gof(poiss.mod)
#par(mfrow=c(1,4))
#plot(poiss.mod)


###
#### modelling non-linear y= ax/(b+x) ####
x = mu.cases.t
mm.mod <- nls(mu.deaths.t ~ a*x/(b+x))
par(mfrow=c(1,4)) # Change the panel layout to 1 x 4
plot(predict(mm.mod), mu.deaths.t)
###
#glm negative binomial
#nb.mod<-glm.nb(mu.deaths.t ~ mu.cases.t,link=log,data=merged)
#summary(nb.mod)
#par(mfrow=c(1,4))
#plot(nb.mod)
###quasipoisson
###
quasi_poiss <- glm(mu.deaths.t ~ mu.cases.t,family = quasipoisson(link="log"),data=merged)
summary(quasi_poiss)
anova(quasi_poiss,test="F")
par(mfrow=c(1,4))
plot(quasi_poiss)
### gam 
gam.mod <- gam(mu.deaths.t ~ s(mu.cases.t),data = merged)
summary(gam.mod)
par(mfrow=c(1,4))
plot(gam.mod)
par(mfrow=c(1,4))
gam.check(gam.mod)
qqPlot(residuals(gam.mod))
cooks.distance(gam.mod)
AIC(gam.mod)
summary(gam.mod)$r.sq
ggplot(merged, aes(mu.cases.t, mu.deaths.t)) + geom_point() + geom_smooth(method = "gam")
###gam with cubic spline
gam.mod1 <- gam(mu.deaths.t ~ s(mu.cases.t,bs="cr"),data = merged)
summary(gam.mod1)
par(mfrow=c(1,4))
plot(gam.mod1)
#loess
gam.mod2 <- loess(mu.deaths.t ~ mu.cases.t,data = merged)
summary(gam.mod2)
ggplot(merged, aes(mu.cases.t, mu.deaths.t)) + geom_point() + geom_smooth(method = "loess")
###the same for 4 countries together
deaths.sum <- (DeathsByCountry["Kuwait",]+ DeathsByCountry["Qatar",]+ DeathsByCountry["United_Arab_Emirates",]+ DeathsByCountry["Saudi_Arabia",])
cases.sum <- (CasesByCountry["Kuwait",]+ CasesByCountry["Qatar",]+ CasesByCountry["United_Arab_Emirates",]+ CasesByCountry["Saudi_Arabia",])
mu.deaths.sum = c()
mu.cases.sum = c()

for (i in (l_max + 1):length(deaths.sum)){
  mu.cases.sum[i-l_max] <- log(1+mean(cases.sum[(i - l_max): i]))  
  mu.deaths.sum[i-l_max] <-log(1+mean(deaths.sum[i]))
}

merged.more.countries = data.frame(mu.deaths.sum, mu.cases.sum)
###linear
lin.mod.more.countries <- glm(mu.deaths.sum ~ mu.cases.sum, data=merged.more.countries)
summary(lin.mod.more.countries)
par(mfrow=c(1,4)) # Change the panel layout to 1 x 4
plot(lin.mod.more.countries)
####

