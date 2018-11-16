setwd("~/Daniele/Università/Magistrale/Secondo Anno/Data Mining/Competizione Bike Sharing")

library(fasttime)
library(lubridate)
library(rpart)
library(dplyr)

train <- read.csv("99.csv", stringsAsFactors=T)
test <- read.csv("100.csv", stringsAsFactors=T)
n<-nrow(train)
m<-nrow(test)

train$count = log1p(train$count)
train$registered = log1p(train$registered)
train$casual = log1p(train$casual)

test$casual<-NA
test$registered<-NA
test$count<-NA

train %>% 
  mutate(datetime = fastPOSIXct(datetime, "GMT")) %>% 
  mutate(hour = hour(datetime),
         month = month(datetime),
         year = year(datetime),
         wday = wday(datetime)) -> train
test %>% 
  mutate(datetime = fastPOSIXct(datetime, "GMT")) %>% 
  mutate(hour = hour(datetime),
         month = month(datetime),
         year = year(datetime),
         wday = wday(datetime)) -> test

train<-train[,-1]
test<-test[,-1]

combi<-rbind(train, test)

combi$new.hours_reg=0
combi$new.hours_reg[combi$hour<6]=1 
combi$new.hours_reg[combi$hour==1 | combi$hour==0]=2
combi$new.hours_reg[combi$hour>=22]=3
combi$new.hours_reg[combi$hour>6 & combi$hour<16]=4
combi$new.hours_reg[combi$hour==20 | combi$hour==21]=4
combi$new.hours_reg[combi$hour==6]=5
combi$new.hours_reg<-as.factor(combi$new.hours_reg)

combi$new.hours_cas=0
combi$new.hours_cas[combi$hour<7]=1 
combi$new.hours_cas[combi$hour==1 | combi$hour==0]=2
combi$new.hours_cas[combi$hour==7]=3
combi$new.hours_cas[combi$hour>7 & combi$hour<20]=4
combi$new.hours_cas[combi$hour>=20]=5
combi$new.hours_cas<-as.factor(combi$new.hours_cas)

combi$new.temp_reg=0
combi$new.temp_reg[combi$temp<13]=1
combi$new.temp_reg[combi$temp>=13 & combi$temp<22]=2
combi$new.temp_reg[combi$temp>=22 & combi$temp<29]=3
combi$new.temp_reg[combi$temp>=29]=4
combi$new.temp_reg<-as.factor(combi$new.temp_reg)

combi$new.temp_cas=0
combi$new.temp_cas[combi$temp<11]=1
combi$new.temp_cas[combi$temp>=11 & combi$temp<15]=2
combi$new.temp_cas[combi$temp>=15 & combi$temp<19]=3
combi$new.temp_cas[combi$temp>=19 & combi$temp<29]=4
combi$new.temp_cas[combi$temp>=29]=5
combi$new.temp_cas<-as.factor(combi$new.temp_cas)

combi$new.atemp_reg=0
combi$new.atemp_reg[combi$atemp<14]=1
combi$new.atemp_reg[combi$atemp>=14 & combi$atemp<30]=2  
combi$new.atemp_reg[combi$atemp>=30]=3
combi$new.atemp_reg<-as.factor(combi$new.atemp_reg)

combi$new.atemp_cas=0
combi$new.atemp_cas[combi$atemp<11]=1
combi$new.atemp_cas[combi$atemp>=11 & combi$atemp<15]=2
combi$new.atemp_cas[combi$atemp>=15 & combi$atemp<19]=3
combi$new.atemp_cas[combi$atemp>=19 & combi$atemp<29]=4
combi$new.atemp_cas[combi$atemp>=29]=5
combi$new.atemp_cas<-as.factor(combi$new.atemp_cas)

combi$div.year<-0
combi$div.year[combi$year=='2011']=1
combi$div.year[combi$year=='2011' & combi$month>4]=2
combi$div.year[combi$year=='2011' & combi$month>8]=3
combi$div.year[combi$year=='2012']=4
combi$div.year[combi$year=='2012' & combi$month>2]=5
combi$div.year[combi$year=='2012' & combi$month>5]=6
combi$div.year[combi$year=='2012' & combi$month>8]=7
combi$div.year<-as.factor(combi$div.year)

combi$day.type=0
combi$day.type[combi$holiday==0 & combi$workingday==0]="Weekend"
combi$day.type[combi$holiday==1]="Holiday"
combi$day.type[combi$holiday==0 & combi$workingday==1]="Working Day"
combi$day.type<-as.factor(combi$day.type)

combi$weekend = 0
combi$weekend[combi$wday==1 | combi$wday==7] = 1  
combi$weekend=as.factor(combi$weekend)

combi$holiday<-as.factor(combi$holiday)
combi$workingday<-as.factor(combi$workingday)

combi$season<-as.factor(combi$season)
levels(combi$season)<-c("Winter", "Spring","Summer", "Fall")

combi$weather[combi$weather=='4']=3   
combi$weather<-as.factor(combi$weather)
levels(combi$weather)<-c("Clear or Few Clouds","Mist", "Snow or Rain + Thunderstorm")

combi$hour<-as.factor(combi$hour)
combi$month<-as.factor(combi$month)
combi$year<-as.factor(combi$year)

combi$wday<-as.factor(combi$wday)
levels(combi$wday)<-c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

train<-combi[1:n,]
test<-combi[(n+1):(n+m),]


fml.reg <- registered ~ season+holiday+workingday+weather+temp+humidity+windspeed+hour+year+wday+new.hours_reg+new.atemp_reg+div.year+day.type+weekend
fml.cas <- casual ~  season+holiday+workingday+weather+temp+humidity+windspeed+hour+year+wday+new.hours_cas+new.atemp_cas+div.year+day.type+weekend


library(caret)

# per il tuning degli iperparametri abbiamo consultato https://xgboost.readthedocs.io/en/latest/index.html e
# https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/

#la griglia usata per il tuning è messa come commento perché ci mette tantissimo tempo a fornire i risultati

#my_grid<-expand.grid(nrounds=c(200,300,400,500), eta=seq(0.01,0.15,by=0.01), max_depth=c(3,4,5,6), gamma=0,
#                     min_child_weight=c(2,3,4,5), subsample=c(0.8,0.9,1), colsample_bytree=c(0.8,0.9,1))

#set.seed(457)

#fit.xgb <- train(fml.reg ~ ., 
#                 train,
#                 method = "xgbTree",
#                 trControl=ctrl,
#                 tuneGrid=my_grid,
#                 metric = "RMSE")

#fit.xgb <- train(fml.cas ~ ., 
#                 train,
#                 method = "xgbTree",
#                 trControl=ctrl,
#                 tuneGrid=my_grid,
#                 metric = "RMSE")

#i risultati ottenuti sulla base dell'ottimizzazione del RMSE hanno portato alle seguenti griglie di parametri:

my_grid.reg<-expand.grid(nrounds=400, eta=0.13, max_depth=5, gamma=0,
                     min_child_weight=4, subsample=1, colsample_bytree=0.9)

my_grid.cas<-expand.grid(nrounds=500, eta=0.08, max_depth=5, gamma=0,
                     min_child_weight=5, subsample=0.9, colsample_bytree=0.8)

ctrl <- trainControl(method = "cv",
                     number = 5)

set.seed(457)

fit.xgb.reg <- train(fml.reg, 
                     train,
                     method = "xgbTree",
                     trControl=ctrl,
                     metric="RMSE",
                     tuneGrid=my_grid.reg)


set.seed(457)

fit.xgb.cas <- train(fml.cas, 
                     train,
                     method = "xgbTree",
                     trControl=ctrl,
                     metric="RMSE",
                     tuneGrid=my_grid.cas)


yhat.xgb.reg <- expm1(predict(fit.xgb.reg, test))
yhat.xgb.cas <- expm1(predict(fit.xgb.cas, test))

yhat_xgb<-yhat.xgb.reg+yhat.xgb.cas

write.table(file="mybike.txt", yhat_xgb, row.names = FALSE, col.names = FALSE)


