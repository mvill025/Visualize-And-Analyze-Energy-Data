# install.packages("forecast")
library(forecast)
## Apply time series linear regression to the sub-meter 3 ts object and use
##  summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")


# SM2 FORECAST

fitSM2 <- tslm(tsSM2_09_daily_1400 ~ trend + season)
forecastfitSM2 <- forecast(
  fitSM2, 
  h = 100, 
  level = c(80,90)
)
plot(forecastfitSM2)


# SM1 FORECAST

fitSM1 <- tslm(tsSM1_06_09_30min ~ trend + season)