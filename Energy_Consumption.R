
#set your working directory
setwd("D:/Users/1015624/GE Internal/My Docs/Work/Energy")

#load required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(plyr)
library(stringr)
library(randomForest)
library(caret)
library(Deducer)
library(lubridate)

#importing dataset
dat <- read.csv("energydata_complete.csv", stringsAsFactors = FALSE)

str(dat)
summary(dat)

#renaming attributes meaningfully
#names(dat)[] <- 'temp_kitchen'
dat <- dat %>% dplyr::rename('temp_kitchen'=T1,'temp_living'=T2,'temp_laundry'=T3,
                                 'temp_office'=T4,'temp_bath'=T5,'temp_build'=T6,'temp_iron'=T7,
                                 'temp_teen'=T8,'temp_parent'=T9,'hum_kitchen'=RH_1,'hum_living'=RH_2,
                                 'hum_laundry'=RH_3,'hum_office'=RH_4,'hum_bath'=RH_5,'hum_build'=RH_6,
                                 'hum_iron'=RH_7,'hum_teen'=RH_8,'hum_parent'=RH_9)

#convert date column to datetime format
dat$date <- strptime(dat$date, format = "%Y-%m-%d %H:%M:%S")
dat$date <- as.POSIXct(dat$date, format = "%Y-%m-%d %H:%M:%S")

dat$hour <- format(dat$date, "%H")
dat$month <- as.factor(months(dat$date))

#hourly consumption month wise
hour_month_usage <- dat %>% group_by(hour, month) %>% dplyr::summarise(usage = sum(Appliances),
                                                                       avg_usage = mean(Appliances))

ggplot(hour_month_usage, aes(x = month, y = hour, fill = avg_usage)) + geom_tile() +
  scale_x_discrete(limits = c('January', 'February', 'March','April','May')) + 
  labs(title = "Average hourly consumption (Month wise)", x = 'Month', y='Hour')+
  theme(plot.title=element_text(hjust=0.5))# + scale_fill_gradient(low="yellow", high="red")
  
#hourly energy consumption on day of the week
dat$day <- wday(dat$date, label = TRUE)

hour_day_usage <- dat %>% group_by(hour, day) %>% dplyr::summarise(usage = sum(Appliances),
                                                                       avg_usage = mean(Appliances))

ggplot(hour_day_usage, aes(x = day, y = hour, fill = avg_usage)) + geom_tile() +
  labs(title = "Average hourly consumption (Day of Week)", x = 'Month', y='Hour')+
  theme(plot.title=element_text(hjust=0.5))# + scale_fill_gradient(low="yellow", high="red")


avg_visibility_hour <- dat %>% group_by(month,hour) %>% dplyr::summarise(avg_visibility = mean(Visibility))
dat %>% group_by(month) %>% dplyr::summarise(avg_visibility = mean(Visibility))

