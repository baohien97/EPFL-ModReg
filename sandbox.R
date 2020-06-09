deaths <- read.csv("latest_data.csv")
DeathsByCountry <- unlist(readRDS("DeathsByCountry.rds")[[1]])
CasesByCountry <- unlist(readRDS("CasesByCountry.rds")[[1]])
CountryPop <- unlist(readRDS("CountryPop.rds")[[1]])
CountryNames <- readRDS("CountryNames.rds")


days.of.data <- 1 + (deaths$day + 31*(deaths$month>1) + 29*(deaths$month>2) + 31*(deaths$month>3) + 30*(deaths$month>4)
                     + 31*(deaths$month>5) + 30*(deaths$month>6) + 31*(deaths$month>7) + 31*(deaths$month>8) 
                     + 30*(deaths$month>9) + 31*(deaths$month>10) + 30*(deaths$month>11) )*(deaths$year==2020) 
n.days <- max(days.of.data)


test = deaths[deaths$continentExp == "Asia",]
countries = as.vector(unique(test$countriesAndTerritories))
ncols.mat = dim(DeathsByCountry)[2]
mat = matrix(ncol=ncols.mat, nrow=length(countries), byrow=F)
row.names(mat) = countries
con_pop = 0
for (i in 1:length(countries)){
  country = countries[i]
  if (is.element(country, row.names(DeathsByCountry))){
    mat[country,] <- DeathsByCountry[country,]
    con_pop = con_pop + data.frame(CountryPop)[country,]
  }
}
n = "America"
cumsum_deaths = cumsum(colSums(mat, T))
# plot(1:length(cumsum_deaths), cumsum_deaths/con_pop, main=n, xlab="Days since 31 December 2019", ylab="Cumulative cases/deaths per million", pch=16,
#      cex=0.9,
#      log="y", ylim=10^c(-1,4),
#      panel.first = { abline(v=1+cumsum(c(1,31,29,31,30,31,30,31)), col="grey", lwd=1.5);   abline(v=7*c(1:52), col="grey", lty=3);
#        abline(h=10^c(-1:4), col="grey") }
#      )

sum_deaths = colSums(mat,T)
plot(1:length(sum_deaths), sum_deaths)

no.cases.countries = c()
for (i in 1:dim(mat)[1]){
  country = rownames(mat)[i]
  if (all(mat[country,] == 0) || all(is.na(mat[country,]))){
    no.cases.countries[i] = country
  }
}
no.cases.countries = na.omit(no.cases.countries)

dimnames <- list(country=rownames(mat), time=1:dim(mat)[2])
mat2 <- matrix(mat, ncol=dim(mat)[2], nrow=dim(mat)[1], dimnames=dimnames)
df = as.data.frame(as.table(mat2))
colnames(df) = c("country", "time", "deaths")
df = na.omit(df)
df = df[!is.element(df$country, no.cases.countries),]
pops = data.frame(CountryPop)
df = merge(df, pops, by.y=0, by.x="country")
qplot(x=time, y=deaths/CountryPop, data = df, geom = "point") +
  facet_wrap(~country)