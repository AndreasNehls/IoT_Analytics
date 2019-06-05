library(RMySQL)
library(plotly)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(dplyr)
library(plyr)
library(readr)
library(doMC)
library(forecast)

### <--- The number of cores to use for parallel execution
registerDoMC(cores = 8)

### Create a database connection
con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

### List the tables contained in the database
dbListTables(con)

### Using the dbListFields function learn the attributes
### associated with the yr_2006 table
dbListFields(con,'yr_2006')

### Use the dbGetQuery function to download tables 2006
### through 2010 with the specified attributes.
yr_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

### add electricity price france c/kWh
yr_2006$price <- 0.1194
yr_2007$price <- 0.1211
yr_2008$price <- 0.1213
yr_2009$price <- 0.1206
yr_2010$price <- 0.1283

### create a new column with the cost for each Sub_metering
yr_2006$Sub_metering_1_cost_ <- with(yr_2006, ((Sub_metering_1)/1000)*price)
yr_2006$Sub_metering_2_cost_ <- with(yr_2006, ((Sub_metering_2)/1000)*price)
yr_2006$Sub_metering_3_cost_ <- with(yr_2006, ((Sub_metering_3)/1000)*price)

yr_2007$Sub_metering_1_cost <- with(yr_2007, ((Sub_metering_1)/1000)*price)
yr_2007$Sub_metering_2_cost <- with(yr_2007, ((Sub_metering_2)/1000)*price)
yr_2007$Sub_metering_3_cost <- with(yr_2007, ((Sub_metering_3)/1000)*price)

yr_2008$Sub_metering_1_cost <- with(yr_2008, ((Sub_metering_1)/1000)*price)
yr_2008$Sub_metering_2_cost <- with(yr_2008, ((Sub_metering_2)/1000)*price)
yr_2008$Sub_metering_3_cost <- with(yr_2008, ((Sub_metering_3)/1000)*price)

yr_2009$Sub_metering_1_cost <- with(yr_2009, ((Sub_metering_1)/1000)*price)
yr_2009$Sub_metering_2_cost <- with(yr_2009, ((Sub_metering_2)/1000)*price)
yr_2009$Sub_metering_3_cost <- with(yr_2009, ((Sub_metering_3)/1000)*price)

yr_2010$Sub_metering_1_cost <- with(yr_2010, ((Sub_metering_1)/1000)*price)
yr_2010$Sub_metering_2_cost <- with(yr_2010, ((Sub_metering_2)/1000)*price)
yr_2010$Sub_metering_3_cost <- with(yr_2010, ((Sub_metering_3)/1000)*price)

### Investigate each new data frame. 
str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)
### start and stop date --> 2006-12-16 17:24:00 to 2006-12-31 23:59:00
### entire year --> incomplete

str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)
### start and stop date --> 2007-01-01 00:00:00 to 2007-12-31 23:59:00
### entire year --> complete

str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)
### start and stop date --> 2008-01-01 00:00:00 to 2008-12-31 23:59:00
### entire year --> complete

str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)
### start and stop date --> 2009-01-01 00:00:00 to 2009-12-31 23:59:00
### entire year --> complete

str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)
### start and stop date --> 2010-01-01 00:05:00 to 2010-11-26 21:02:00
### entire year --> incomplete

### data type
### $ id                   : num
### $ Date                 : chr
### $ Time                 : chr
### $ Global_active_power  : num
### $ Global_reactive_power: num
### $ Global_intensity     : num
### $ Voltage              : num
### $ Sub_metering_1       : num
### $ Sub_metering_2       : num
### $ Sub_metering_3       : num
### $ price                : num
### $ Sub_metering_1_cost  : num
### $ Sub_metering_2_cost  : num
### $ Sub_metering_3_cost  : num

### Combine tables into one dataframe using dplyr
df <- bind_rows(yr_2007,yr_2008,yr_2009)
head(df)

str(df)
summary(df)
head(df)
tail(df)
### start and stop date --> 2007-01-01 00:00:00 to 2009-12-31 23:59:00
### entire year --> complete
### $ id                   : num
### $ Date                 : chr
### $ Time                 : chr
### $ Global_active_power  : num
### $ Global_reactive_power: num
### $ Global_intensity     : num
### $ Voltage              : num
### $ Sub_metering_1       : num
### $ Sub_metering_2       : num
### $ Sub_metering_3       : num
### $ price                : num
### $ Sub_metering_1_cost  : num
### $ Sub_metering_2_cost  : num
### $ Sub_metering_3_cost  : num


### Combine Date and Time attribute values in a new attribute column
df <- cbind(df, paste (df$Date, df$Time), stringsAsFactors = FALSE)
str(df)

### Give the new attribute in the 14th column a header name
colnames(df)[15] <-"DateTime"
str(df)

### Move the DateTime attribute within the dataset
df <- df[,c(ncol(df), 1:(ncol(df)-1))]
str(df)

### Convert DateTime from POSIXlt to POSIXct 
df$DateTime <- as.POSIXct(df$DateTime, "%Y/%m/%d %H:%M:%S")

### Add the time zone
attr(df$DateTime, "tzone") <- "Europe/Paris"

### Inspect the data types
str(df)
summary(df)
head(df)
tail(df)

### Create a dataframe for the temperature (every 3 hour) and wrote as csv file
weather_df <- read_delim("7149.01.01.2007.31.12.2010.1.0.0.en.ansi.00000000.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE, skip = 6)
### select column of interest
weather <- select(weather_df ,'Local time in Paris / Orly (airport)', 'T', 'U')
### rename column
weather <- setNames(weather, c("DateTime","Temperatur", "RelativeHumidity"))
### change date/time format
DatesTime <- format(as.POSIXct(strptime(weather$DateTime,"%d.%m.%Y %H:%M",tz="")) ,format = "%Y-%m-%d %H:%M:%S")
weather$DateTime <- DatesTime

### order the DateTime column
### join both dataframes (household_power_consumption.txt[df] + 7149.01.01.2007.31.12.2010.1.0.0.en.ansi.00000000.csv[weather])
df_com <- join(weather, df, type="right")
sum(is.na(df_com$Temperatur))
sum(is.na(df_com$RelativeHumidity))

# Sample data
x <- df_com$Temperatur
goodIdx <- !is.na(x)
goodIdx
#>  [1] FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
#> [15]  TRUE  TRUE FALSE FALSE
# These are the non-NA values from x only
# Add a leading NA for later use when we index into this vector
goodValsX <- c(NA, x[goodIdx])
goodValsX
# Fill the indices of the output vector with the indices pulled from
# these offsets of goodVals. Add 1 to avoid indexing to zero.
fillIdx <- cumsum(goodIdx)+1
fillIdx
# The original vector with gaps filled
goodValsX[fillIdx]
df_com$Temperatur <- goodVals[fillIdx]

# Sample data
y <- df_com$RelativeHumidity
goodIdy <- !is.na(y)
goodIdy
# These are the non-NA values from x only
# Add a leading NA for later use when we index into this vector
goodValsy <- c(NA, y[goodIdy])
goodValsy
# Fill the indices of the output vector with the indices pulled from
# these offsets of goodVals. Add 1 to avoid indexing to zero.
fillIdy <- cumsum(goodIdy)+1
fillIdy
# The original vector with gaps filled
goodValsy[fillIdy]
df_com$RelativeHumidity <- goodValsy[fillIdy]


### export my data as a csv file with the price, cost and temperatur column
write.csv(df_com, file='household_power_consumption.csv')
df <- df_com

### Create "year" attribute with lubridate
df$year <- year(df$DateTime)

### Create "month" attribute with lubridate
df$month <- month(df$DateTime)

### Create "week" attribute with lubridate
df$week <- week(df$DateTime)

### Create "weekDay" attribute with lubridate
df$weekDay <- weekdays(df$DateTime)

### Create "day" attribute with lubridate
df$day <- day(df$DateTime)

### Create "hour" attribute with lubridate
df$hour <- hour(df$DateTime)

### Create "minute" attribute with lubridate
df$minute <- minute(df$DateTime)



#### 1. Visualize the data ####

### Granularity
### Plot all of sub-meter 1
plot(df$Sub_metering_1)


### Subsetting and Meaningful Time Periods
### Subset the second week of 2008 - All Observations
houseWeek <- filter(df, year == 2008 & week == 2)
### Plot subset houseWeek
plot(houseWeek$Sub_metering_1)


### Visualize a Single Day with Plotly
### Subset the 9th day of January 2008 - All observations
houseDay <- filter(df, year == 2008 & month == 1 & day == 9)
### Plot sub-meter 1
plot_ly(houseDay,
        x = ~houseDay$DateTime,
        y = ~houseDay$Sub_metering_1,
        type = 'scatter',
        mode = 'lines')
### Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


### Reducing Granularity
### Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(df, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))
### Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

### Subset the 2007 - 2010 - All observations
houseYear <- filter(df, year > 2005)

plot_ly(houseYear, x = ~houseYear$DateTime, y = ~houseYear$Sub_metering_1, name = 'SUB-METER 1', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption - SUB-METER 1",
         xaxis = list(title = "Time"),
         yaxis = list (title = "kwh"))

plot_ly(houseYear, x = ~houseYear$DateTime, y = ~houseYear$Sub_metering_2, name = 'SUB-METER 2', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption - SUB-METER 2",
         xaxis = list(title = "Time"),
         yaxis = list (title = "kwh"))

plot_ly(houseYear, x = ~houseYear$DateTime, y = ~houseYear$Sub_metering_3, name = 'SUB-METER 3', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption - SUB-METER 3",
         xaxis = list(title = "Time"),
         yaxis = list (title = "kwh"))

#### 1. Visualize the data  - Produce Two More Visualizations ####
#### Now that you understand the workflow, it’s your turn to create two additional visualizations to
#### compliment one that features "Day". All three visualizations along with your analysis will be
#### needed for your report to the homebuilder. 
####
#### Visualizations needed for your report
#### The Day day visualization you built in the walkthrough above
### Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(df, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))
### Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008 - 10 Minute frequency",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
####
#### Create a visualization with plotly for a Week of your choosing. Use all three sub-meters and
#### make sure to label. Experiment with granularity
### Subset the 2nd Calendar Week of 2008 - 30 Minute frequency
houseWeekMinute <- filter(df, year == 2008 & week == 2 & (minute == 0 | minute == 30))
### Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseWeekMinute, x = ~houseWeekMinute$DateTime, y = ~houseWeekMinute$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeekMinute$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeekMinute$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2nd Calendar Week of 2008 - 30 Minute frequency",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
###
#### Create a visualization for a time period of your choice. Both "Day" and "Week" highlight typical
#### patterns in a home. What might be another period of time that could provide insights? Use plotly
#### and experiment with granularity until you find the visualization that maximizes information gain
#### for the viewer.
### Subset the January of 2008 - 1 Hour frequency
houseMonthHour <- filter(df, year == 2008 & month == 1 & (hour == 1 | hour == 2 | hour == 3 | hour == 4 | hour == 5 | hour == 6 | hour == 7 | hour == 8 | hour == 9 | hour == 10 | hour == 11 | hour == 12 | hour == 13 | hour == 14 | hour == 15 | hour == 16 | hour == 17 | hour == 18 | hour == 19 | hour == 20 | hour == 21 | hour == 22 | hour == 23 | hour == 0))
### Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseMonthHour, x = ~houseMonthHour$DateTime, y = ~houseMonthHour$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonthHour$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonthHour$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseMonthHour$Temperatur, name = 'Temperatur', mode = 'lines') %>%
  layout(title = "Power Consumption January of 2008 - 1 Hour frequency",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours) - Temperatur °C"))
### Subset the 2nd Calendar Week of 2008 - 30 Minute frequency
houseWeekMinute <- filter(df, year == 2008 & week == 2 & (minute == 0 | minute == 30))
### Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseWeekMinute, x = ~houseWeekMinute$DateTime, y = ~houseWeekMinute$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeekMinute$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeekMinute$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseWeekMinute$Temperatur, name = 'Temperatur', mode = 'lines') %>%
  layout(title = "Power Consumption 2nd Calendar Week of 2008 - 30 Minute frequency",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours) - Temperatur °C"))
### Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(df, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))
### Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Temperatur, name = 'Temperatur', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008 - 10 Minute frequency",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours) - Temperatur °C"))
###
###
### Subset the August of 2008 - 1 Hour frequency
houseMonthHour <- filter(df, year == 2008 & month == 8 & (hour == 1 | hour == 2 | hour == 3 | hour == 4 | hour == 5 | hour == 6 | hour == 7 | hour == 8 | hour == 9 | hour == 10 | hour == 11 | hour == 12 | hour == 13 | hour == 14 | hour == 15 | hour == 16 | hour == 17 | hour == 18 | hour == 19 | hour == 20 | hour == 21 | hour == 22 | hour == 23 | hour == 0))
### Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseMonthHour, x = ~houseMonthHour$DateTime, y = ~houseMonthHour$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonthHour$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonthHour$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseMonthHour$Temperatur, name = 'Temperatur', mode = 'lines') %>%
  layout(title = "Power Consumption - August, 2008 - 1 Hour frequency",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours) - Temperatur °C"))
### Subset the 32nd Calendar Week of 2008 - 30 Minute frequency
houseWeekMinute <- filter(df, year == 2008 & week == 32 & (minute == 0 | minute == 30))
### Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseWeekMinute, x = ~houseWeekMinute$DateTime, y = ~houseWeekMinute$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeekMinute$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeekMinute$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseWeekMinute$Temperatur, name = 'Temperatur', mode = 'lines') %>%
  layout(title = "Power Consumption - 32nd Calendar Week, 2008 - 30 Minute frequency",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours) - Temperatur °C"))
### Subset the 9th day of August 2008 - 10 Minute frequency
houseDay10 <- filter(df, year == 2008 & month == 8 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))
### Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Temperatur, name = 'Temperatur', mode = 'lines') %>%
  layout(title = "Power Consumption - August 9th, 2008 - 10 Minute frequency",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours) - Temperatur °C"))
####
#### Optional Work 
#### Produce pie chart visualizations that are likely to provide insight, e.g.,
houseYear <- filter(df, year == 2007)
SB1year <- (sum(houseYear$Sub_metering_1))
SB2year <- (sum(houseYear$Sub_metering_2))
SB3year <- (sum(houseYear$Sub_metering_3))
# Simple Pie Chart
SBcategory <- c(SB1year, SB2year, SB2year)
SBlabel <- c("Kitchen", "Laundry Room", "Water Heater & AC")

plot_ly(df, labels = SBlabel, values = SBcategory, type = 'pie', sort = FALSE) %>%
  layout(title = 'Percentage of total power usage')

#### Percentage of total use at various times of day by each sub-meter.
#### Percentage of total power use over a day by each sub-meter.
#### Percentage of total power use over an entire year by each sub-meter.
#### Produce any other visualizations that you believe may provide insight.

#### 2. Prepare to analyze the data ####

### Store your data frame(s) as time series
### Subset to one observation per week on Mondays at 8:00pm (20:00) for 2007, 2008 and 2009
house070809weekly <- filter(df, weekDay == 'Monday' & hour == 20 & minute == 1)
### Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

### Produce time series plots
### Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM3_070809weekly)
### Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
 ### Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)

#### 2. Prepare to analyze the data  - Produce Two More Visualizations ####
#### Produce Two More Visualizations
#### It’s your turn to create two additional ts objects and plot them to compliment sub-meter 3 from
#### above. All three visualizations along with your analysis will be needed for your report to the
#### homebuilder.
#### 
#### Visualizations needed for your report
#### The sub-meter 3 plot you built in the walkthrough above
### Store your data frame(s) as time series
house070809weekly <- filter(df, weekDay == 'Monday' & hour == 20 & minute == 1)
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))
autoplot(tsSM3_070809weekly, colour = 'green', xlab = "Time", ylab = "Power (watt-hours)", main = "Power Consumption - Water Heater & AC - per week on Mondays at 8:00pm for 2007, 2008 and 2009")
####
#### Sub-meter 1 with your choice of frequency and time period
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency=52, start=c(2007,1))
autoplot(tsSM1_070809weekly, colour = 'blue', xlab = "Time", ylab = "Power (watt-hours)", main = "Power Consumption - Kitchen - per week on Mondays at 8:00pm for 2007, 2008 and 2009")
####
#### Sub-meter 2 with your choice of frequency and time period
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency=52, start=c(2007,1))
autoplot(tsSM2_070809weekly, colour = 'orange', xlab = "Time", ylab = "Power (watt-hours)", main = "Power Consumption - Laundry Room - per week on Mondays at 8:00pm for 2007, 2008 and 2009")
####


#### 3. Forecasting a time series ####

### Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)
### Create the forecast for sub-meter 3. Forecast ahead 52 time periods 
forecastfitSM3 <- forecast(fitSM3, h=52)
### Plot the forecast for sub-meter 3.
plot(forecastfitSM3)
### Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=52, level=c(80,90))
### Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Power (watt-hours)", xlab="Time")

#### 3. Forecasting a time series  - Produce Two More Forecast ####
#### It’s your turn to create two additional forecasts and plot them to compliment sub-meter 3 from
#### above. All three visualizations along with your analysis will be needed for your report to the
#### homebuilder.
#### 
#### Visualizations and analysis needed for your report:
#### A sub-meter 3 plot you built in the walkthrough above
### Apply time series linear regression to the sub-meter 3
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)
forecastfitSM3 <- forecast(fitSM3, h=52)
plot(forecastfitSM3)
forecastfitSM3c <- forecast(fitSM3, h=52, level=c(80,90))
### Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(-2, 30), ylab= "Power (watt-hours)", xlab="Time", main="Forecast from Linear regression model (1 year) - Water Heater & AC")
####
#### Sub-meter 1 with your choice of frequency, time period and confidence levels
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
summary(fitSM1)
forecastfitSM1 <- forecast(fitSM1, h=52)
plot(forecastfitSM1)
forecastfitSM1c <- forecast(fitSM1, h=52, level=c(80,90))
plot(forecastfitSM1c, ylim = c(-2, 30), ylab= "Power (watt-hours)", xlab="Time", main="Forecast from Linear regression model (1 year) - Kitchen")
#### Sub-meter 2 with your choice of frequency, time period and confidence levels
fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) 
summary(fitSM2)
forecastfitSM2 <- forecast(fitSM2, h=52)
plot(forecastfitSM2)
forecastfitSM2c <- forecast(fitSM2, h=52, level=c(80,90))
plot(forecastfitSM2c, ylim = c(-2, 30), ylab= "Power (watt-hours)", xlab="Time", main="Forecast from Linear regression model (1 year) - Laundry Room")
####
#### One comparison chart showing the R2 and RMSE of each model you built
accuracy(fitSM1)
accuracy(fitSM2)
accuracy(fitSM3)




#### 4. Decomposing a Seasonal Time Series ####
#### Decomposition
### Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
### Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
### Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

#### 4. Decomposing a Seasonal Time Series  - Produce Two More Visualizations ####
#### It’s your turn to create two additional decomposed ts objects and plot them to compliment
#### sub-meter 3 from above. All three visualizations along with your analysis will be needed
#### for your report to the homebuilder.
#### 
#### Visualizations and analysis needed for your report:
#### The sub-meter 3 decomposed plot you built in the walkthrough above
### Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
plot(components070809SM3weekly)
summary(components070809SM3weekly)
#### Sub-meter 1 decomposed plot with your choice of frequency and time period
components070809SM1weekly <- decompose(tsSM1_070809weekly)
plot(components070809SM1weekly)
summary(components070809SM1weekly)
#### Sub-meter 2 decomposed plot with your choice of frequency and time
components070809SM2weekly <- decompose(tsSM2_070809weekly)
plot(components070809SM2weekly)
summary(components070809SM2weekly)
#### One comparison chart showing the summary statistics for the seasonal, trend and remainder
#### components from each decomposed object


#### 5. Holt-Winters Forecasting ####
#### Remove Seasonal Components
### Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
### Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

#### HoltWinters Simple Exponential Smoothing
### Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(-2, 30))

#### HoltWinters Forecast
### HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=52)
plot(tsSM3_HW070809for, ylim = c(-2, 30), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
### Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=52, level=c(10,25))
### Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(-2, 30), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

#### 5. Holt-Winters Forecasting  - Produce Two More Visualizations ####
### It’s your turn to create two additional sets of HoltWinters forecast plots to compliment sub-meter 3
### from above. All visualizations along with your analysis will be needed for your report to the homebuilder.
### 
### Visualizations and analysis needed for your report:
### The sub-meter 3 forecast plot and a plot containing only the forecasted area from the walkthrough above
### Plot only the forecasted area
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
plot(decompose(tsSM3_070809Adjusted))
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(-2, 30))
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=52)
plot(tsSM3_HW070809for, ylim = c(-2, 30), ylab= "Watt-Hours", xlab="Time - Water Heater & AC")
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=52, level=c(10,25))
plot(tsSM3_HW070809forC, ylim = c(-2, 30), ylab= "Watt-Hours", xlab="Time - Water Heater & AC", start(2010))
### Sub-meter 1 forecast plot and a plot containing only the forecasted area. Your choice of frequency and time period.
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)
plot(decompose(tsSM1_070809Adjusted))
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(-2, 30))
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=52)
plot(tsSM1_HW070809for, ylim = c(-2, 30), ylab= "Watt-Hours", xlab="Time - Kitchen")
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=52, level=c(10,25))
plot(tsSM1_HW070809forC, ylim = c(-2, 30), ylab= "Watt-Hours", xlab="Time - Kitchen", start(2010))
### Sub-meter 2 forecast plot and a plot containing only the forecasted area. Your choice of frequency and time period.
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)
plot(decompose(tsSM2_070809Adjusted))
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(-2, 30))
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=52)
plot(tsSM2_HW070809for, ylim = c(-2, 30), ylab= "Watt-Hours", xlab="Time - Laundry Room")
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=52, level=c(10,25))
plot(tsSM2_HW070809forC, ylim = c(-2, 30), ylab= "Watt-Hours", xlab="Time - Laundry Room", start(2010))

