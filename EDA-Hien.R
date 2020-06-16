# Exploratory data analysis : nature of covariates and distribution of response, presence of outliers
# or missing values, range of variables.

source("main.R") # get the function plot.Country
library(gam) # not default lib in my current version of R
library(ggplot2)

deaths <- read.csv("latest_data.csv")
DeathsByCountry <- unlist(readRDS("DeathsByCountry.rds")[[1]])
CasesByCountry <- unlist(readRDS("CasesByCountry.rds")[[1]])
CountryPop <- unlist(readRDS("CountryPop.rds")[[1]])
CountryNames <- readRDS("CountryNames.rds")

  
# Explore some patterns in America, Africa, Asia and some countries in these three continents

# generic function
deaths.cases.Continent = function(continent, chosen.countries=NULL, pop=CountryPop, 
                                  names=CountryNames, plot=FALSE
                                  ){
  
  all.deaths = deaths[deaths$continentExp == continent,]
  countries = as.vector(unique(all.deaths$countriesAndTerritories))
  
  ncols.deaths.mat = dim(DeathsByCountry)[2]
  ncols.cases.mat = dim(CasesByCountry)[2]
  mat.deaths = matrix(ncol=ncols.deaths.mat, nrow=length(countries), byrow=F)
  #print(mat.deaths)
  mat.cases = matrix(ncol=ncols.cases.mat, nrow=length(countries), byrow=F)
  #print(mat.cases)
  row.names(mat.deaths) = countries
  row.names(mat.cases) = countries
  con.pop = 0
  
  for (i in 1:length(countries)){
    country = countries[i]
    if (is.element(country, row.names(DeathsByCountry))){
      mat.deaths[country,] = DeathsByCountry[country,]
      #print(country)
      con.pop = con.pop + data.frame(CountryPop)[country,]
    }
    if (is.element(country, row.names(CasesByCountry))){
      mat.cases[country,] = CasesByCountry[country,]
    }
  }
  

  if (plot){
    n.days <- dim(mat)[2]
    if (is.null(chosen.countries)){
      par(mfrow=c(1,2))
    }
    else {
      par(mfrow=c(2,2))
    }
    # cases/deaths per million
    y <- colSums(mat.deaths, T)
    m <- colSums(mat.cases, T)
    x <- c(1:n.days)
    sub.m <- x[m>0]
    sub.y <- x[y>0]
    xmin=1
    plot(sub.m, m[sub.m]/con.pop, main=continent, xlab="Days since 31 December 2019", 
         ylab="Cases/deaths per million", pch=16, cex=0.9, xlim=c(xmin,n.days),
         log="y", ylim=10^c(-1,4), 
         panel.first = { abline(v=1+cumsum(c(1,31,29,31,30,31,30,31)), col="grey", lwd=1.5);   
           abline(v=7*c(1:52), col="grey", lty=3); 
           abline(h=10^c(-1:4), col="grey") })
    points(sub.y, y[sub.y]/con.pop, cex=0.9, col="red")
    
    # cumulative cases/deaths per million
    y <- cumsum(colSums(mat.deaths, T))
    m <- cumsum(colSums(mat.cases, T))
    x <- c(1:n.days)
    # sub.m <- x[m>0]
    # sub.y <- x[y>0]
    xmin=1
    plot(x, m/con.pop, main=continent, xlab="Days since 31 December 2019",
         ylab="Cumulative cases/deaths per million", pch=16, cex=0.9, xlim=c(xmin,n.days),
         log="y", ylim=10^c(-1,4),
         panel.first = { abline(v=1+cumsum(c(1,31,29,31,30,31,30,31)), col="grey", lwd=1.5);
           abline(v=7*c(1:52), col="grey", lty=3);
           abline(h=10^c(-1:4), col="grey") })
    points(sub.y, y[sub.y]/con.pop, cex=0.9, col="red")
    
    # if some countries are chosen, plot them to compare with average
    if (is.null(chosen.countries) == FALSE){
      print("True")
      avg.con.deaths = colMeans(mat.deaths, T)
      avg.con.cases = colMeans(mat.cases, T)
      plot(1:n.days, avg.con.cases/con.pop, ylab="Cases per million compared with continent's avg", 
           pch=16, cex=0.9,
           log="y", ylim=10^c(-1,4))
      cl = rainbow(length(chosen.countries))
      for (i in 1:length(chosen.countries)){
        print(mat.cases[chosen.countries[i]])
        points(1:n.days, mat.cases[chosen.countries[i],]/pop[countries[i]], col=cl[i], pch="+")
      }
      # legend(1,1000, legend=c("continent's average", chosen.countries))
      plot(1:n.days, avg.con.deaths/con.pop, ylab="Deaths per million compared with continent's avg",
           pch=16, cex=0.9,
           log="y", ylim=10^c(-1,4))
      for (i in 1:length(chosen.countries)){
        points(1:n.days, mat.deaths[chosen.countries[i],]/pop[countries[i]], col=cl[i], pch="+")
      }
    }
  }
  return (list(mat.cases, mat.deaths))
}

deaths.cases.Continent("America", c("Chile", "Brazil", "Canada"))
#deaths.cases.Continent("Asia")
#deaths.cases.Continent("Africa")

# Plot the time series of new cases/deaths every day in each country
plot.all.countries.Continent = function(continent, 
                                        pop=CountryPop, names=CountryNames){
  cases = na.omit(unlist(deaths.cases.Continent(continent)[1][[1]]))
  # print(cases)
  #print(cases)
  deaths = na.omit(unlist(deaths.cases.Continent(continent)[2][[1]]))
  pop = data.frame(CountryPop)
  colnames(pop) = c("country.pop")
  no.cases.countries = c()
  for (i in 1:dim(mat)[1]){
    country = rownames(cases)[i]
    #print(country)
    if (all(cases[country,] == 0) || all(is.na(cases[country,]))){
      no.cases.countries[i] = country
    }
  }
  no.cases.countries = na.omit(no.cases.countries)
  
  dimnames <- list(country=rownames(cases), time=1:dim(cases)[2])
  mat <- matrix(cases, ncol=dim(cases)[2], nrow=dim(cases)[1], dimnames=dimnames)
  df.cases = as.data.frame(as.table(mat))
  colnames(df.cases) = c("country", "time", "num")
  df.cases = na.omit(df.cases)
  # print(df)
  
  dimnames <- list(country=rownames(deaths), time=1:dim(deaths)[2])
  mat <- matrix(deaths, ncol=dim(deaths)[2], nrow=dim(deaths)[1], dimnames=dimnames)
  df.deaths = as.data.frame(as.table(mat))
  colnames(df.deaths) = c("country", "time", "num")

  df.cases$type = "cases"
  df.deaths$type = "deaths"
  
  df = rbind(df.cases, df.deaths)
  df = df[!is.element(df$country, no.cases.countries),]
  df = merge(df, pop, by.y=0, by.x="country")
  #print(pop)
  ggplot(df, aes(x=time, y=num, col=type)) + geom_point() + 
    facet_wrap(~country, scales = "free_y", shrink = TRUE,
               labeller = "label_value", as.table = TRUE, drop = TRUE)
}


# Distribution of cases/deaths in a day
dist.deaths.cases.Continent = function(continent, day=NULL, pop=CountryPop, names=CountryNames){
  
  cases = na.omit(unlist(deaths.cases.Continent(continent)[1][[1]]))
  # print(cases)
  #print(cases)
  deaths = na.omit(unlist(deaths.cases.Continent(continent)[2][[1]]))
  pop = data.frame(pop)
  colnames(pop) = c("country.pop")
  
  if (is.null(day)){
    day = which.max(colSums(cases, T)) # take the day with the highest number of cases
  }
  dimnames <- list(country=rownames(cases), time=1:dim(cases)[2])
  mat <- matrix(cases, ncol=dim(cases)[2], nrow=dim(cases)[1], dimnames=dimnames)
  df.cases = as.data.frame(as.table(mat))
  colnames(df.cases) = c("country", "time", "num")
  df.cases = na.omit(df.cases[df.cases$time == day, ])
  
  dimnames <- list(country=rownames(deaths), time=1:dim(deaths)[2])
  mat <- matrix(deaths, ncol=dim(deaths)[2], nrow=dim(deaths)[1], dimnames=dimnames)
  df.deaths = as.data.frame(as.table(mat))
  colnames(df.deaths) = c("country", "time", "num")
  df.deaths = na.omit(df.deaths[df.deaths$time == day, ])
  df.cases$type = "cases"
  df.deaths$type = "deaths"
  df = rbind(df.cases, df.deaths)
  df = merge(df, pop, by.y=0, by.x="country")
  ggplot(df, aes(x=num, fill=type)) +
  # + geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
  #                                          binwidth=.5,
  #                                          colour="black", fill="white") 
  geom_histogram(alpha=.2) 
}


# plot.covariates