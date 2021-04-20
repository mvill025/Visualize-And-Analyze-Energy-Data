# install.packages("forecast")
library(forecast)

## LINEAR REGRESSION

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
  h = 30 * 2,
  level = c(80,95)
)
plot(forecastfitSM2)


# SM1 FORECAST

fitSM1 <- tslm(
  tsSM1_10_First12Weeks_30min ~ trend + season
)
forecastfitSM1 <- forecast(
  fitSM1,
  h = (2 * 24 * 7) * 2,
  level = c(80,95)
)
plot(forecastfitSM1)

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
plot(components070809SM3weekly)
summary(components070809SM3weekly)

## Decompose Sub-meter 2 into trend, seasonal and remainder
components09daily1400 <- decompose(tsSM2_09_daily_1400)
plot(components09daily1400)
summary(components09daily1400)

## Decompose Sub-meter 1 into trend, season and remainder
components10First12Weeks30min <- decompose(tsSM1_10_First12Weeks_30min)
plot(components10First12Weeks30min)


## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- 
  tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## HOLT WINTERS

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(
  tsSM3_HW070809for, 
  ylim = c(0, 20), 
  ylab = "Watt-Hours", 
  xlab = "Time - Sub-meter 3"
)

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(
  tsSM3_HW070809, 
  h=25, 
  level=c(10,25)
)

## Plot only the forecasted area
plot(
  tsSM3_HW070809forC, 
  ylim = c(0, 20), 
  ylab = "Watt-Hours", 
  xlab = "Time - Sub-meter 3", 
  start(2010)
)

## SM2 HOLT WINTERS

tsSM2_09daily1400_adj <- tsSM2_09_daily_1400 - components09daily1400$seasonal
plot(tsSM2_09daily1400_adj)
tsSM2_HW_09daily1400_adj <- HoltWinters(
  tsSM2_09daily1400_adj, 
  beta = FALSE,
  gamma = FALSE
)
plot(tsSM2_HW_09daily1400_adj)
tsSM2_HW_09daily1400_adj_for <- forecast(
  tsSM2_HW_09daily1400_adj, 
  h = 30
)
tsSM2_HW_09daily1400_adj_forc <- forecast(
  tsSM2_HW_09daily1400_adj, 
  h = 30,
  level = c(10,25)
)
## Plot only the forecasted area
plot(
  tsSM2_HW_09daily1400_adj_for, 
  ylim = c(-10, 30), 
  ylab = "Watt-Hours", 
  xlab = "Time - Sub-meter 2", 
)

## SM1 HOLT WINTERS
tsSM1_10_First12Weeks_30min_adj <- 
  tsSM1_10_First12Weeks_30min - components10First12Weeks30min$seasonal
plot(tsSM1_10_First12Weeks_30min_adj)
tsSM1_HW_10_First12Weeks_30min_adj <- HoltWinters(
  tsSM1_10_First12Weeks_30min_adj,
  beta = FALSE,
  gamma = FALSE,
)
plot(tsSM1_HW_10_First12Weeks_30min_adj)
tsSM1_HW_10_First12Weeks_30min_adj_for <- forecast(
  tsSM1_HW_10_First12Weeks_30min_adj,
  h = (2 * 24 * 7) * 2
)
plot(
  tsSM1_HW_10_First12Weeks_30min_adj_for
)
 