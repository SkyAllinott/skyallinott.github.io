# Working Directory and Packages ----

setwd("C:/Users/Bret/OneDrive/R Projects/GDP Forecast/")
library(tseries)
library(forecast)


# Cansim to time series function ----
ts_cansim <- function(vector,start, end=NA) {
  require(cansim)
  require(lubridate)
  
  if(is.na(end)){
    xx <- get_cansim_vector(vector, start_time=start)
  } else {
    xx <- get_cansim_vector(vector, start_time=start, end_time = end)
  }
  
  time.diff <- as.Date(xx$REF_DATE[2]) - as.Date(xx$REF_DATE[1])
  
  x <- xx$VALUE
  n <- nrow(xx)
  
  st.date <- as.Date(xx$REF_DATE[1])
  end.date <- as.Date(xx$REF_DATE[n])
  
  if (1 <= time.diff & time.diff <=4) {
    n.frequency = 365.25
    x <- ts(xx$VALUE, start = c(year(st.date), month(st.date), day(st.date)), freq= n.frequency)
  }
  if (time.diff == 7){
    n.frequency = 365.25/7
    x <- ts(xx$VALUE, start = c(year(st.date), month(st.date), day(st.date)), freq= n.frequency)
  }
  if (25 < time.diff & time.diff <= 31){
    n.frequency = 12
    x <- ts(xx$VALUE, start= c(year(st.date), month(st.date)), fre=n.frequency)
  }
  if (70 < time.diff & time.diff <= 100){
    n.frequency=4
    x <- ts(xx$VALUE, start= c(year(st.date), quarter(st.date)), freq=n.frequency)
  }
  if (300 < time.diff & time.diff <= 400){
    n.frequency=1
    x <- ts(xx$VALUE, start=year(st.date), freq=n.frequency)
  }
  return(x)
}

# ARMA Model Construction ----
# Data Acquisition and Plot
monGDP <- ts_cansim('v65201210', '1997-01-01')
plot(log(monGDP))

#Detrend with Year on Year Growth Rate
monGDPyoy <- diff(log(monGDP), 12)
plot(monGDPyoy)

#Determinining Optimal order of the ARMA componenets
mod.monGDP <- auto.arima(monGDPyoy, seasonal= FALSE, stationary = TRUE)
summary(mod.monGDP)

#Optiaml model is ARMA(1,1)
plot(monGDPyoy) +
  lines(fitted(mod.monGDP),col='blue')

# Model Validation Statistics ----

#Testing for stationarity with Dickey Fuller test

#H0: Unit root precense. 
reg.lin <- lm(monGDP ~ time(monGDP))
ttrend <- ts(reg.lin$fitted.values, freq=12, start= start(monGDP))
monGDP.tt <- monGDP - ttrend

adf.test(monGDP.tt, alternative = c("stationary"))

## We fail to reject the null hypothesis that there is a unit root.

# Testing for serial correlation in our optimal model

plot(mod.monGDP$residuals)
plot(density(mod.monGDP$residuals))

ehat <- mod.monGDP$residuals
ehatmod <- Arima(ehat, order = c(1,0,0), include.mean = FALSE)
summary(ehatmod)
## s.e. is twice as big as coefficient (absolute terms. This is insignificant. Ehat_t does  
# not correlate with Ehat_{t-1}. We conclude no serial correlation.

# Forecasting GDP ----
autoplot(monGDPyoy)
fc.gdp <- forecast(mod.monGDP, h=15)
autoplot(fc.gdp, include=60,
         main='ARMA(1,1) Forecast of Monthly Canadian GDP Until Dec 2021',
         ylab= 'Monthly GDP (year on year, millions)',
         xlab= 'Time')








