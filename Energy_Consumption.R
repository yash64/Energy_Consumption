
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

#importing datasets
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

str(test)
summary(train)

#names(train)[] <- 'temp_kitchen'

train <- train %>% dplyr::rename('temp_kitchen'=T1,'temp_living'=T2,'temp_laundry'=T3,
                                 'temp_office'=T4,'temp_bath'=T5,'temp_build'=T6,'temp_iron'=T7,
                                 'temp_teen'=T8,'temp_parent'=T9,'hum_kitchen'=RH_1,'hum_living'=RH_2,
                                 'hum_laundry'=RH_3,'hum_office'=RH_4,'hum_bath'=RH_5,'hum_build'=RH_6,
                                 'hum_iron'=RH_7,'hum_teen'=RH_8,'hum_parent'=RH_9)
test <- test %>% dplyr::rename('temp_kitchen'=T1,'temp_living'=T2,'temp_laundry'=T3,
                               'temp_office'=T4,'temp_bath'=T5,'temp_build'=T6,'temp_iron'=T7,
                               'temp_teen'=T8,'temp_parent'=T9,'hum_kitchen'=RH_1,'hum_living'=RH_2,
                               'hum_laundry'=RH_3,'hum_office'=RH_4,'hum_bath'=RH_5,'hum_build'=RH_6,
                               'hum_iron'=RH_7,'hum_teen'=RH_8,'hum_parent'=RH_9)
