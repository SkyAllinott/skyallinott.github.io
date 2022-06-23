setwd("C:/Users/Bret/OneDrive/R Projects/CoronaVirus Modelling/")

mydata <- read.csv("COVIDdata.csv",header=TRUE)
attach(mydata)

library(stringr)
library(dplyr)
library(lubridate)

#Canada
canada <- mydata[which(str_detect(mydata$countriesAndTerritories, "Canada")), ]
canadasorted <- canada[nrow(canada):1,]
date.start <- as.Date("2019-12-31")
vec.st <- c(year(date.start), month(date.start), day(date.start))
canadacases <- ts(cumsum(canadasorted$cases), start=vec.st, freq=365)


#USA
USA <- mydata[which(str_detect(mydata$countriesAndTerritories, "United_States_of_America")), ]
USAsorted <- USA[nrow(USA):1,]
USAcases <- ts(cumsum(USAsorted$cases), start=vec.st, freq=365)

#Italy
italy <- mydata[which(str_detect(mydata$countriesAndTerritories, "Italy")), ]
italysorted <- italy[nrow(italy):1,]
italycases <- ts(cumsum(italysorted$cases), start=vec.st, freq=365)

#South Korea
sKorea <- mydata[which(str_detect(mydata$countriesAndTerritories, "South_Korea")), ]
sKoreasorted <- sKorea[nrow(sKorea):1,]
sKoreacases <- ts(cumsum(sKoreasorted$cases), start=vec.st, freq=365)

#Plotting Cases
par(mfrow=c(4,1))
plot(canadacases,
     pch= 19,
     main="Canadian Cases",
     xlab="Date",
     ylab="Total Cases",
     col="steelblue",
)
plot(USAcases,
     main="USA Cases",
     pch= 19,
     xlab="Date",
     ylab="Total Cases",
     col="red",
)
plot(italycases,
     main="Italy Cases",
     pch= 19,
     xlab="Date",
     ylab="Total Cases",
     col="darkgreen",
)
plot(sKoreacases,
     main="South Korea Cases",
     pch= 19,
     xlab="Date",
     ylab="Total Cases",
     col="orange",
)
