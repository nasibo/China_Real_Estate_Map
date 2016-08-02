# Name: China_Housing
# Date Created:June 14th 2016
# Date Last Modified: July 19th 2016
# Created By: Spike Nowak
# Modified By: 
# Last Modified By:
# Uses Data: China_Res_Housing_Monthly_Prices.csv
# Description: This file takes in monthly data on Chinese housing prices in 70 cities
#   and transforms the data to annualized data for use in a D3 map

# bring in relavent packages
library(dplyr)
library(tidyr)
library(EDAWR)
library(ggmap)
library(jsonlite)
library(Quandl)

# set the working directory
setwd("/Users/apple/Desktop/China_Housing")


# read in the data
data <- read.csv("China_Res_Housing_Monthly_Prices.csv", 
                  stringsAsFactors = FALSE)

# remove the last column of the data because it has no data
data <- data[,1:ncol(data)-1]

## put the column names that you will want, in this case the city names, 
## in an object for after transposing the data. Transpose has the horrible feature
## that if fucks with the column names
colnames <- data[,1]

## Now transpose the data, but don't include the city names because you've alredy
## saved them in the object "colnames"
data <- as.data.frame(t(data[,2:ncol(data)]))

## now add in those city names for the column names
colnames(data) <- colnames

## The dates are strange in the uploaded filed so lets change them to something
## more managable 

new_dates <- seq(as.Date("2011-01-01"), as.Date("2016-04-01"), by="month")
new_dates <- sort(new_dates, decreasing = TRUE)
new_dates <- c("Date", new_dates)


## then attach those row names to the data
rownames(data) <- new_dates

## add last row as index
spike <- data.frame(x = 2, y = 5)

last_row <- as.data.frame(matrix(rep(100,ncol(data)), nrow = 1, ncol = ncol(data)))
rownames(last_row) <- as.Date("2010-12-01")
colnames(last_row) <- colnames
data <- rbind(data, last_row)
percents <- (data - 100)/100

## ok so now we have 2 dataframes, one with our data in the form is was downloaded 
## in and another in percent form. Working with 2 dataframes is hard, so I'll 
## gather the data into long columns

## make a new column of dates

dates <- seq(as.Date("2010-12-01"), as.Date("2016-04-01"), by="month")
dates <- sort(dates, decreasing = TRUE)
dates <- as.data.frame(dates)
colnames(dates) <- "Date"

## lets change the dates to just the year and the month
dates <- format(dates, "%b%Y")

new_data <- cbind(dates, data)

long_data <- gather(new_data,"City", "Value",2:ncol(new_data))

cities <- unique(long_data$City)

## this for look will create a data frame where each monthly data point for 
## each city is index to December 2010. 
indexed_city_data <- NULL
for( i in 1:length(cities)){
  city <- cities[i]
  city_data <- long_data[long_data$City==city,]
  percents <- city_data$Value
  percents <- (city_data$Value - 100)/100
  percentsDF <- data.frame(percents = percents, order = 1:length(percents))
  percentsDF <- arrange(percentsDF, desc(order))
  percents <- percentsDF$percents
  index_start <- 100
    for( p in 1:(length(percents)-1)){
      new_index <- index_start[p] * (1 + percents[p + 1])
      index_start <- c(index_start, new_index) 
    }
  indexed_data <- index_start
  df <- data.frame(Value = indexed_data, Order = 1:length(indexed_data))
  df <- arrange(df, desc(Order))
  city_data$Value <- df$Value
  indexed_city_data <- rbind(indexed_city_data, city_data)
}

all_city_data <- spread(indexed_city_data, Date, Value)
##all_city_data <- arrange(all_city_data, Date) 

## get cities long and lat
cities <- unique(all_city_data$City)
longlat <- geocode(cities)

## attach long and lat to housing data
all_city_data$long <- longlat$lon
all_city_data$lat <- longlat$lat

write.csv(all_city_data, "Indexed_China_Housing_to_April_2016.csv")

