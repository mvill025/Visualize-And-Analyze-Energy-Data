library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

## Create a database connection 
con = dbConnect(
  MySQL(),
  user = 'deepAnalytics', 
  password = 'Sqltask1234!', 
  dbname = 'dataanalytics2018', 
  host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com'
)

# Data Set Information
# - This archive contains 2075259 measurements gathered in a house located in 
#   Sceaux (7km of Paris, France) between December 2006 and November 2010 
#   (47 months).
# Notes:
#   1.(global_active_power*1000/60 - sub_metering_1 - sub_metering_2 - sub_metering_3) 
#     represents the active energy consumed every minute (in watt hour) in the 
#     household by electrical equipment not measured in sub-meterings 1, 2 and 
#     3
#   2.The dataset contains some missing values in the measurements 
#     (nearly 1,25% of the rows). All calendar timestamps are present in the 
#     dataset but for some timestamps, the measurement values are missing: a 
#     missing value is represented by the absence of value between two 
#     consecutive semi-colon attribute separators. For instance, the dataset 
#     shows missing values on April 28, 2007.

# 
# Attribute Information:
#   
# 1.date: Date in format dd/mm/yyyy
# 2.time: time in format hh:mm:ss
# 3.global_active_power: household global minute-averaged active power 
#   (in kilowatt)
# 4.global_reactive_power: household global minute-averaged reactive power 
#   (in kilowatt)
# 5.voltage: minute-averaged voltage (in volt)
# 6.global_intensity: household global minute-averaged current intensity 
#   (in ampere)
#
# 7.sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
#   It corresponds to the kitchen, containing mainly a dishwasher, an oven and a
#   microwave (hot plates are not electric but gas powered).
#
# 8.sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). 
#   It corresponds to the laundry room, containing a washing-machine, a 
#   tumble-drier, a refrigerator and a light.
#
# 9.sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
#   It corresponds to an electric water-heater and an air-conditioner.

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

# yr_2006 attributes 
dbListFields(con, 'yr_2006')
# yr_2007 attributes 
dbListFields(con, 'yr_2007')
# yr_2008 attributes 
dbListFields(con, 'yr_2008')
# yr_2009 attributes 
dbListFields(con, 'yr_2009')
# yr_2010 attributes 
dbListFields(con, 'yr_2010')

# getting data from each table
fetchSubMeterData <- function (tableName) {
  response <- dbGetQuery(
    con, 
    paste(
      "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM ",
      tableName
    )
  )
  return(response)
}
yr_2006 <- fetchSubMeterData("yr_2006")
yr_2007 <- fetchSubMeterData("yr_2007")
yr_2008 <- fetchSubMeterData("yr_2008")
yr_2009 <- fetchSubMeterData("yr_2009")
yr_2010 <- fetchSubMeterData("yr_2010")

# investigate each data frame
investigate <- function (df) {
  print(str(df))
  print(summary(df))
  print(head(df))
  print(tail(df))
}

investigate(yr_2006)
investigate(yr_2007)
investigate(yr_2008)
investigate(yr_2009)
investigate(yr_2010)

# create primary data frame with all the data
SUB_METERING_2006_2010 <- bind_rows(
  yr_2006,
  yr_2007,
  yr_2008,
  yr_2009,
  yr_2010,
)

# make sure rows are in order
investigate(SUB_METERING_2006_2010)

# combine Date and Time into a DateTime Column with a DateTime data type

## Combine Date and Time attribute values in a new attribute column
SUB_METERING_2006_2010 <- 
  cbind(
    SUB_METERING_2006_2010,
    paste(
      SUB_METERING_2006_2010$Date,
      SUB_METERING_2006_2010$Time
    ), 
    stringsAsFactors = FALSE
  )

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(SUB_METERING_2006_2010)[6] <-"DateTime"

## Convert DateTime from character to POSIXct 
SUB_METERING_2006_2010$DateTime <- 
  as.POSIXct(
    SUB_METERING_2006_2010$DateTime, 
    "%Y/%m/%d %H:%M:%S"
  )

# adding the time zone
attr(SUB_METERING_2006_2010$DateTime, "tzone") <- "Europe/Paris"

# Create "year" attribute with lubridate
SUB_METERING_2006_2010$Year <- year(SUB_METERING_2006_2010$DateTime)
# Create "quarter" attribute with lubridate 
SUB_METERING_2006_2010$Quarter <- quarter(SUB_METERING_2006_2010$DateTime)
# Create "month" attribute with lubridate        
SUB_METERING_2006_2010$Month <- month(SUB_METERING_2006_2010$DateTime)
# Create "week" attribute with lubridate
SUB_METERING_2006_2010$Week <- week(SUB_METERING_2006_2010$DateTime)
# Create "weekday" attribute with lubridate
SUB_METERING_2006_2010$Weekday <- wday(SUB_METERING_2006_2010$DateTime)
# Create "day" attribute with lubridate
SUB_METERING_2006_2010$Day <- day(SUB_METERING_2006_2010$DateTime)
# Create "hour" attribute with lubridate
SUB_METERING_2006_2010$Hour <- hour(SUB_METERING_2006_2010$DateTime)
# Create "minute" attribute with lubridate
SUB_METERING_2006_2010$Minute <- minute(SUB_METERING_2006_2010$DateTime)

# inspect data
investigate(SUB_METERING_2006_2010)
summary(SUB_METERING_2006_2010)

# write to local file just in case :P
write.csv(SUB_METERING_2006_2010,"SUB_METERING_2006_2010.csv")

# additional EDA

# meter 1:  
# It corresponds to the kitchen, containing mainly a dishwasher, an oven and a 
# microwave (hot plates are not electric but gas powered)
summary(SUB_METERING_2006_2010$Sub_metering_1)

# meter 2:  
# It corresponds to the laundry room, containing a washing-machine, a 
# tumble-drier, a refrigerator and a light.
summary(SUB_METERING_2006_2010$Sub_metering_2)

# meter 3:  
# It corresponds to an electric water-heater and an air-conditioner.
summary(SUB_METERING_2006_2010$Sub_metering_3)

# splitting data based on seasons
WinterData <- subset(SUB_METERING_2006_2010, Month == 12 | Month == 1 | Month == 2)
SpringData <- subset(SUB_METERING_2006_2010, Month >= 3 & Month <= 5)
SummerData <- subset(SUB_METERING_2006_2010, Month >= 6 & Month <= 8)
FallData <- subset(SUB_METERING_2006_2010, Month >= 9 & Month <= 11)

# correlation and covariance matrix
numericalOnly <- function(df) {
  newDF <- df 
  newDF$Date <- NULL
  newDF$Time <- NULL
  newDF$DateTime <- NULL
  return(newDF)
}

cor(numericalOnly(SUB_METERING_2006_2010))
cov(numericalOnly(SUB_METERING_2006_2010))

print(summary(WinterData))
print(summary(SpringData))
print(summary(SummerData))
print(summary(FallData))

# compare cor matrices and cov matrices
df <- WinterData
cor( numericalOnly( SUB_METERING_2006_2010 ) )
cor( numericalOnly( df ) )
cov( numericalOnly( SUB_METERING_2006_2010 ) )
cov( numericalOnly( df ) )

# create cor heat maps
cormat <- round(cor( numericalOnly( SUB_METERING_2006_2010 ) ),2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(
  melted_cormat, 
  aes(Var2, Var1, fill = value)
) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(
    midpoint = 0, 
    limit = c(-1,1), 
    space = "Lab", 
    name="SUB_METERING_2006_2010 \nCorrelation"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = 
      element_text(
        angle = 45, 
        vjust = 1, 
        size = 12, 
        hjust = 1
      )
  ) + 
  coord_fixed()
# Print the heatmap
print(ggheatmap)
