
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
library(tidyr)

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
                                 'hum_iron'=RH_7,'hum_teen'=RH_8,'hum_parent'=RH_9) %>%
  mutate(month = floor_date(date(date), unit = 'months'))

#convert date column to datetime format
dat$date <- strptime(dat$date, format = "%Y-%m-%d %H:%M:%S")
dat$date <- as.POSIXct(dat$date, format = "%Y-%m-%d %H:%M:%S")

dat$hour <- format(dat$date, "%H")
#dat$month <- as.factor(months(dat$date))

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

temparature <- dat %>%
  group_by(month) %>%
  dplyr::summarise(min_temp_kitch=min(temp_kitchen),
            max_temp_kitch=max(temp_kitchen),
            min_temp_living=min(temp_living),
            max_temp_living=max(temp_living),
            min_temp_laundry=min(temp_laundry),
            max_temp_laundry=max(temp_laundry),
            min_temp_iron=min(temp_iron),
            max_temp_iron=max(temp_iron),
            min_temp_office=min(temp_office),
            max_temp_office=max(temp_office),
            min_temp_bath=min(temp_bath),
            max_temp_bath=max(temp_bath),
            min_temp_parent=min(temp_parent),
            max_temp_parent=max(temp_parent),
            min_temp_teen=min(temp_teen),
            max_temp_teen=max(temp_teen))


# temp2 <- temparature %>%
#   tidyr::gather(temp_min_max, Temp, -month)

# ggplot() + 
#   geom_area(data = temp2 %>% filter(temp_min_max %in% c('min_temp_kitch', 'max_temp_kitch')),
#             aes(x=month,y=Temp,fill = temp_min_max, color = temp_min_max),
#             position = 'identity') +
#   geom_point(data = temp2 %>% filter(temp_min_max %in% c('min_temp_kitch', 'max_temp_kitch')), aes(x=month, y=Temp))


#min-max temperature in each room
ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_kitch,ymax=max_temp_kitch), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_kitch), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_kitch), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_kitch), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_kitch), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Kitchen)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_living,ymax=max_temp_living), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_living), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_living), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_living), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_living), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Living Room)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_laundry,ymax=max_temp_laundry), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_laundry), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_laundry), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_laundry), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_laundry), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Laundry)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_iron,ymax=max_temp_iron), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_iron), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_iron), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_iron), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_iron), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Iron Room)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_office,ymax=max_temp_office), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_office), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_office), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_office), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_office), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Office)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_bath,ymax=max_temp_bath), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_bath), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_bath), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_bath), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_bath), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Bathroom)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_parent,ymax=max_temp_parent), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_parent), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_parent), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_parent), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_parent), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Parent Room)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_teen,ymax=max_temp_teen), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_teen), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_teen), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_teen), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_teen), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Teen Room)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))

#wind speed across months
#ggplot(dat, aes(x=month, y=Windspeed))+ stat_summary(fun.y = 'mean', geom = 'line')
ggplot(dat, aes(x=hour, y=Windspeed))+ stat_summary(fun.y = 'mean', geom = 'point')

wind <- dat%>%group_by(month)%>%dplyr::summarise(speed_avg=mean(Windspeed))
ggplot(wind, aes(x=month,y=speed_avg)) +geom_point(colour='orange')+geom_line(colour='light coral',linetype=2)+
  labs(title='Wind speed m/s',x='Month',y='Average Wind Speed')+
  theme(plot.title = element_text(hjust=0.5))

#wind speed each hour per month
wind_hour <- dat%>%group_by(hour,month)%>%dplyr::summarise(speed_avg=mean(Windspeed))
ggplot(wind_hour,aes(x=hour,y=speed_avg,group=month,colour=factor(month, labels = c('Jan','Feb','Mar','Apr','May'))))+
  geom_point() + geom_line()+scale_colour_discrete(name = "Month") +
  labs(title='Wind speed m/s',x='Hour',y='Average Wind Speed')+
  theme(plot.title = element_text(hjust=0.5))
  

