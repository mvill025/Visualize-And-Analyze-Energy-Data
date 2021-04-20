# install.packages("ggfortify")
# install.poackages("ggplot2")
library(ggplot2)
library(ggfortify)

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(
  SUB_METERING_2006_2010, Weekday == 2 & Hour == 20 & Minute == 1
)

## Create TS object with SubMeter3
## Season = 1 year
tsSM3_070809weekly <- ts(
  house070809weekly$Sub_metering_3, 
  frequency=52, 
  start=c(2007,1)
)

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(
  tsSM3_070809weekly, 
  color = 'red', 
  xlab = "Time", 
  ylab = "Watt Hours", 
  main = "Sub-meter 3"
)

## Season = 3 months
house_09_daily_1400 <- filter(
  SUB_METERING_2006_2010, 
  Year == 2009 & Hour == 14 & Minute == 1
)
tsSM2_09_daily_1400 <- ts(
  house_09_daily_1400$Sub_metering_2,
  start = c(0,1),
  frequency = 30 * 3,
)
autoplot(
  tsSM2_09_daily_1400, 
  color = 'green', 
  xlab = "Time", 
  ylab = "Watt Hours", 
  main = "Sub-meter 2 (Laundry) Daily, 2009"
)

## season = 1 Weeks
house_10_First12Weeks_30min <- filter(
  SUB_METERING_2006_2010, 
  Year == 2010 & Week <= 12 & Minute %% 30 == 0
)
tsSM1_10_First12Weeks_30min <- ts(
  house_10_First12Weeks_30min$Sub_metering_1,
  start = c(1,1),
  frequency = 2 * 23 * 7,
)
autoplot(
  tsSM1_10_First12Weeks_30min, 
  color = 'blue', 
  xlab = "Time", 
  ylab = "Watt Hours", 
  main = "Sub-meter 1 (Kitchen) 30 mintues, First 12 Weeks 2010"
)
