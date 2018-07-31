
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
library(corrplot)
library(dummies)
library(e1071)

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
dat$day <- wday(dat$date, label = TRUE)

#hourly consumption month wise
hour_month_usage <- dat %>% group_by(hour, month) %>% dplyr::summarise(usage = sum(Appliances),
                                                                       avg_usage = mean(Appliances))

# jpeg('Appliances_histogram_boxplot2_Jan29.jpg',width = 14, height = 10, units = 'in', res = 300)
# par(mfrow=c(2,1))
# hist(dat$Appliances,main="",xlab = "Appliances Wh",breaks = 40,
#      col='lightblue',xlim=c(0,1200),ylim=c(0,9000),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

hist(dat$Appliances, col = 'light coral', xlab = 'Appliances (Wh)')

g1<-ggplot(dat, aes(x=day, y=Appliances))+ stat_summary(fun.y = 'mean',geom='bar')
ggsave("Appliances usage_day.jpg",plot = g1, width = 8, height = 5)
g2<-ggplot(dat, aes(x=month, y=Appliances))+ stat_summary(fun.y = 'mean', geom = 'bar')
ggsave("Appliances usage_month.jpg",plot = g2, width = 8, height = 5)

dat$month <- as.factor(dat$month)

g3 <- ggplot(hour_month_usage, aes(x = factor(month,labels = c('January', 'February', 'March','April','May')), y = hour, fill = avg_usage)) + geom_tile() +
  #xlim('January', 'February', 'March','April','May') + 
  #scale_x_discrete(limits = c('January', 'February', 'March','April','May')) + 
  labs(title = "Average hourly consumption (Month wise)", x = 'Month', y='Hour')+
  theme(plot.title=element_text(hjust=0.5))# + scale_fill_gradient(low="yellow", high="red")
ggsave("Hourly consumption.jpg",plot = g3, width = 8, height = 5)

#hourly energy consumption on day of the week


hour_day_usage <- dat %>% group_by(hour, day) %>% dplyr::summarise(usage = sum(Appliances),
                                                                   avg_usage = mean(Appliances))

g4<-ggplot(hour_day_usage, aes(x = day, y = hour, fill = avg_usage)) + geom_tile() +
  labs(title = "Average hourly consumption (Day of Week)", x = 'Month', y='Hour')+
  theme(plot.title=element_text(hjust=0.5))# + scale_fill_gradient(low="yellow", high="red")
ggsave("Hourly consumption_day of week.jpg",plot = g4, width = 8, height = 5)

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
g5<-ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_kitch,ymax=max_temp_kitch), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_kitch), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_kitch), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_kitch), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_kitch), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Kitchen)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("temperature_kitchen.jpg",plot = g5, width = 8, height = 5)

g6<-ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_living,ymax=max_temp_living), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_living), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_living), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_living), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_living), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Living Room)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("temperature_living.jpg",plot = g6, width = 8, height = 5)

g7<-ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_laundry,ymax=max_temp_laundry), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_laundry), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_laundry), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_laundry), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_laundry), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Laundry)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("temperature_laundry.jpg",plot = g7, width = 8, height = 5)

g8<-ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_iron,ymax=max_temp_iron), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_iron), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_iron), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_iron), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_iron), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Iron Room)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("temperature_iron.jpg",plot = g8, width = 8, height = 5)

g9<-ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_office,ymax=max_temp_office), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_office), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_office), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_office), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_office), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Office)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("temperature_office.jpg",plot = g9, width = 8, height = 5)

g10<-ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_bath,ymax=max_temp_bath), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_bath), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_bath), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_bath), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_bath), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Bathroom)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("temperature_bathroom.jpg",plot = g10, width = 8, height = 5)

g11<-ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_parent,ymax=max_temp_parent), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_parent), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_parent), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_parent), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_parent), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Parent Room)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("temperature_parent.jpg",plot = g11, width = 8, height = 5)

g12<-ggplot(data = temparature) + 
  geom_ribbon(aes(x=month, ymin=min_temp_teen,ymax=max_temp_teen), fill='light coral')+
  geom_point(aes(x=month, y=min_temp_teen), colour='light sea green')+
  geom_point(aes(x=month, y=max_temp_teen), colour='tan4')+
  geom_line(aes(x=month,y=min_temp_teen), colour='light sea green', size=0.9)+
  geom_line(aes(x=month,y=max_temp_teen), colour='tan4', size=0.9)+
  labs(title='Min-Max temperature (Teen Room)',x='Month',y='Temperature')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("temperature_teen.jpg",plot = g12, width = 8, height = 5)

#wind speed across months
#ggplot(dat, aes(x=month, y=Windspeed))+ stat_summary(fun.y = 'mean', geom = 'line')
g13<-ggplot(dat, aes(x=hour, y=Windspeed))+ stat_summary(fun.y = 'mean', geom = 'point')+
  labs(title='Wind speed m/s',x='Hour',y='Average Wind Speed')+
  theme(plot.title = element_text(hjust=0.5))
ggsave("windSpeed_hour.jpg",plot = g13, width = 8, height = 5)

wind <- dat%>%group_by(month)%>%dplyr::summarise(speed_avg=mean(Windspeed))
g14<-ggplot(wind, aes(x=month,y=speed_avg)) +geom_point(colour='orange')+geom_line(colour='light coral',linetype=2)+
  labs(title='Wind speed m/s',x='Month',y='Average Wind Speed')+
  theme(plot.title = element_text(hjust=0.5))
ggsave("windSpeed_month.jpg",plot = g14, width = 8, height = 5)

#wind speed each hour per month
wind_hour <- dat%>%group_by(hour,month)%>%dplyr::summarise(speed_avg=mean(Windspeed))
g15<-ggplot(wind_hour,aes(x=hour,y=speed_avg,group=month,colour=factor(month, labels = c('Jan','Feb','Mar','Apr','May'))))+
  geom_point() + geom_line()+scale_colour_discrete(name = "Month") +
  labs(title='Wind speed m/s',x='Hour',y='Average Wind Speed')+
  theme(plot.title = element_text(hjust=0.5))
ggsave("hourly wind speed.jpg",plot = g15, width = 8, height = 5)


#creating model for energy prediction  
dat$month <- as.factor(dat$month)

#create train and test datasets(70:30)
set.seed(1)

#copy of original dataframe
dat_model <- dat

dat_model$rv1 <- NULL
dat_model$rv2 <- NULL
#dat_model$date <- NULL


#correlation plot
#corrplot(cor(dat_model[,2:26]), method = 'number', type = 'upper',order = 'hclust',number.cex=0.65, tl.cex = 0.9)
corrplot(cor(dat_model[,2:26]),type='upper',method='color',tl.cex = 0.9)

index <- sample(nrow(dat_model), size = nrow(dat_model)*0.7)
train <- dat_model[index,]
test <- dat_model[-(index),]

#include dummy variables
train_dat <- dummy.data.frame(train, names=c('month','day'))

rf_model <- randomForest(data = train, Appliances ~.-month-day, ntree = 1000, nodesize=10)
print(rf_model)
varImpPlot(rf_model)

lr_model <- lm(data = train, Appliances ~.-month-day)
summary(lr_model)

##SVM model
svm_model <- svm(Appliances ~. -month-day, data = train)
sqrt(mean((svm_model$residuals)^2))
train$Appliances-svm_model$


dat$minute <- minute(dat$date)
dat$wday <- ifelse(dat$day == 'Sun' | dat$day == 'Sat','wend','wday')
