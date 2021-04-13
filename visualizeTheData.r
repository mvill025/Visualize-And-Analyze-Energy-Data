library(plotly)
library(ggplot2)


plot(SUB_METERING_2006_2010$Sub_metering_1)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(SUB_METERING_2006_2010, Year == 2008 & Week == 2)
## Plot subset houseWeek
plot(houseWeek$DateTime, houseWeek$Sub_metering_1)

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(SUB_METERING_2006_2010, Year == 2008 & Month == 1 & Day == 9)
## Plot sub-meter 1
plot_ly(
  houseDay, 
  x = ~houseDay$DateTime, 
  y = ~houseDay$Sub_metering_1, 
  type = 'scatter', 
  mode = 'lines'
)

plotSubMeteringAll <- function(df, title) {
  plot_ly(
    df, 
    x = ~df$DateTime, 
    y = ~df$Sub_metering_1, 
    name = 'Kitchen', 
    type = 'scatter', 
    mode = 'lines'
  ) %>% add_trace(
    y = ~df$Sub_metering_2, 
    name = 'Laundry Room', 
    mode = 'lines'
  ) %>% add_trace(
    y = ~df$Sub_metering_3, 
    name = 'Water Heater & AC',
    mode = 'lines'
  ) %>% layout(
    title = title,
    xaxis = list(title = "Time"),
    yaxis = list (title = "Power (watt-hours)")
  )
}

oneDay <- filter(SUB_METERING_2006_2010, Year == 2008 & Month == 1 & Day == 9)
## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plotSubMeteringAll(oneDay)

## Subset the 9th day of January 2008 - 10 Minute frequency
day10 <- filter(
  SUB_METERING_2006_2010,
  Year == 2008 & Month == 1 & Day == 9 & Minute %% 15 == 0
)
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plotSubMeteringAll(day10, "Power Consumption January 9th, 2008")

oneWeek <- filter(
  SUB_METERING_2006_2010,
  Year == 2008 & Month == 1 & Week == 2 & Minute %% 15 == 0
)

twoMonths <- filter(
  SUB_METERING_2006_2010,
  Year == 2008 & (Month == 1 | Month == 2) & Minute %% 1 == 0
)

plotSubMeteringAll(twoMonths, "Power Consumption January - February, 2008")