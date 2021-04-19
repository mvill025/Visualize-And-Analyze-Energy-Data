# install.packages("ggfortify")
# install.poackages("ggplot2")
library(ggplot2)
library(ggfortify)

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(
  SUB_METERING_2006_2010, Weekday == 2 & Hour == 20 & Minute == 1
)

## Create TS object with SubMeter3
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
  ts.colour = 'red', 
  xlab = "Time", 
  ylab = "Watt Hours", 
  main = "Sub-meter 3"
)

house_06_09_30min <- filter(
  SUB_METERING_2006_2010, Year >= 2006 & Year <= 2009 & Minute %% 30 == 0
)
tsSM1_06_09_30min <- ts(
  house_06_09_30min$Sub_metering_1,
  start = c(2006,1),
  frequency = 2 * 24 * 365,
)
autoplot(
  tsSM1_06_09_30min, 
  ts.colour = 'blue', 
  xlab = "Time", 
  ylab = "Watt Hours", 
  main = "Sub-meter 1 (Kitchen) 30 mintues, 2006-2009"
)

house_09_daily_1400 <- filter(
  SUB_METERING_2006_2010, 
  Year == 2009 & Hour == 14 & Minute == 5
)
str(house_09_daily_1400)
tsSM2_09_daily_1400 <- ts(
  house_09_daily_1400$Sub_metering_2,
  start = c(0,1),
  frequency = 30,
)
autoplot(
  tsSM2_09_daily_1400, 
  ts.colour = 'green', 
  xlab = "Time", 
  ylab = "Watt Hours", 
  main = "Sub-meter 2 (Laundry) Daily, 2009"
)

