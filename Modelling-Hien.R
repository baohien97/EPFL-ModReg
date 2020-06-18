source("main.R") # get the function plot.Country
library(gam) # not default lib in my current version of R
library(ggplot2)

deaths <- read.csv("latest_data.csv")
DeathsByCountry <- unlist(readRDS("DeathsByCountry.rds")[[1]])
CasesByCountry <- unlist(readRDS("CasesByCountry.rds")[[1]])
CountryPop <- unlist(readRDS("CountryPop.rds")[[1]])
CountryNames <- readRDS("CountryNames.rds")


# We model number of deaths as a fraction of cases confirmed on l previous 
# days (smoothened)

l_max = 14 # number of previous days to take into account

country = "United_Arab_Emirates"
deaths.country = (DeathsByCountry[country,])
cases.country = (CasesByCountry[country,])
plot(cases.country, deaths.country, main=country, xlab="cases", ylab="deaths")
abline(lm(deaths.country~cases.country), col="red")

mu.deaths.t = c() 
mu.cases.t = c()


for (i in (l_max + 1):length(deaths.country)){
  mu.cases.t[i-l_max] <- mean(cases.country[(i - l_max): i]) 
  mu.deaths.t[i-l_max] <- deaths.country[i]
}

merged = data.frame(mu.deaths.t, mu.cases.t)
#### identity link #### 
lin.mod <- glm(mu.deaths.t ~ mu.cases.t, data=merged)
summary(lin.mod)
AIC(lin.mod)
par(mfrow=c(1,4)) # Change the panel layout to 1 x 4
plot(lin.mod)
plot(predict(lin.mod))
lines(mu.deaths.t)

#### poisson ####
pois.mod <- glm(mu.deaths.t ~ log(mu.cases.t), family=poisson(link=log), data=merged)
summary(pois.mod)
AIC(pois.mod)
par(mfrow=c(1,4)) # Change the panel layout to 1 x 4
plot(pois.mod)
plot(predict(pois.mod))
lines(mu.deaths.t)

#### quasipoisson #### 
quasi.poiss <- glm(mu.deaths.t ~ log(mu.cases.t + 1),family = quasipoisson(link="log"),data=merged)
summary(quasi.poiss)
AIC(quasi.poiss)
anova(quasi.poiss,test="F")
par(mfrow=c(1,4))
plot(quasi_poiss)


#### modelling non-linear y= ax/(b+x) ####
x = mu.cases.t
mm.mod <- nls(mu.deaths.t ~ a*x/(b+x))
par(mfrow=c(1,4)) # Change the panel layout to 1 x 4
plot(predict(mm.mod), mu.deaths.t)
AIC(mm.mod)

#### modelling gam ####
gam.mod <- gam(mu.deaths.t ~ s(mu.cases.t), family=poisson(link="log"), data = merged) 
summary(gam.mod)
par(mfrow=c(1,4))
plot(gam.mod)
par(mfrow=c(1,4))
gam.check(gam.mod)
qqPlot(residuals(gam.mod))
#cooks.distance(gam.mod)
AIC(gam.mod)
summary(gam.mod)$r.sq
ggplot(merged, aes(mu.cases.t, mu.deaths.t)) + geom_point() + geom_smooth(method = "gam")
