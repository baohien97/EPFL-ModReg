library(readxl)
library(httr)

library(utils)


deaths <- read_excel("latest_data_18062020.xlsx")
# deaths$dateRep <- as.Date(deaths$dateRep)
# deaths <- subset(deaths, dateRep >= "2020-06-01")


CountryNames <- unlist(unique(deaths$countriesAndTerritories),use.names=F)  # in alphabetical order
CountryId <- match(CountryNames, unlist(deaths$countriesAndTerritories,use.names=F))
# CountryPop <- match(CountryNames, unlist(deaths[,10]))/10^6  # 2018 populations in millions
# names(CountryPop) <- CountryNames

CountryId <- unlist(deaths$geoId,use.names=F)[CountryId]
CountryId[CountryId=="UK"] <- "GB"      # country for UK is wrong
CountryId[CountryId=="EL"] <- "GR"      # ditto Greece
CountryId[CountryId=="PYF"] <- "PF"     # ditto French Polynesia
CountryId[CountryId=="NA"] <- "NA"     # Namibia is NA !

# CountryId <- CountryId[CountryId!=c("GE","JE","XK","JPG11668","AN")] 

nc <- length(CountryNames)
# days starting from and including 31 December 2019 (code will work OK up to end of 2020)
days.of.data <- 1 + (deaths$day + 31*(deaths$month>1) + 29*(deaths$month>2) + 31*(deaths$month>3) + 30*(deaths$month>4)
                     + 31*(deaths$month>5) + 30*(deaths$month>6) + 31*(deaths$month>7) + 31*(deaths$month>8) 
                     + 30*(deaths$month>9) + 31*(deaths$month>10) + 30*(deaths$month>11) )*(deaths$year==2020) 
n.days <- max(days.of.data)

# rounds a 3-day moving average of a vector of counts to the nearest integer
rounded.moving.average <- function(x, smooth=F)
{  # weight first and last days arbitrarily as (2,1)/3 and (1,2)/3, and the rest as (1,1,1)/3
  n <- length(x)
  z <- filter(x, filter=rep(1, 3)/3)  # leaves NAs as z[1] and z[n]
  z[1] <- (2*x[1]+x[2])/3  
  z[n] <- (x[n-1]+2*x[n])/3
  out <- round(z)
  if (!smooth) out <- x  # output original data if smooth=F
  out
}
# set up matrices with countries by rows and time as columns

DeathsByCountry <- matrix(0, nc, n.days, dimnames = list(CountryNames))
CasesByCountry <- matrix(0, nc, n.days, dimnames = list(CountryNames))

smooth.ts <- T  # change to F to analyse original time series, leave as T to smooth time series

for (i in 1:nc) 
{ i.days <- c(1:nrow(deaths))[deaths[,7]==CountryNames[i]]  # rows corresponding to country i

DeathsByCountry[i, days.of.data[i.days]] <- unlist(deaths[i.days, 6])
DeathsByCountry[i,] <- rounded.moving.average(DeathsByCountry[i,], smooth=smooth.ts)  # round to ensure integer numbers

CasesByCountry[i, days.of.data[i.days]] <- unlist(deaths[i.days, 5])
CasesByCountry[i,] <- rounded.moving.average(CasesByCountry[i,], smooth=smooth.ts)  # round to ensure integer numbers
}

days.rest = 154:171

DeathsByCountry = DeathsByCountry[,days.rest]
CasesByCountry = CasesByCountry[,days.rest]

all_datas = list(DeathsByCountry, CasesByCountry)
all_datas_names = c("DeathsByCountryNew", "CasesByCountryNew")

# write data in files 
for (i in 1:length(all_datas)){
  # print(i)
  saveRDS(all_datas[i], paste(all_datas_names[i], ".rds", sep=""))
}