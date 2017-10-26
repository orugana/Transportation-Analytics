length_distance$rte_points<-paste(as.character(length_distance$TP1),as.character(length_distance$TP2),sep = "-")

# default mtry
data3$uniqueid<-paste(as.character(data3$date),as.character(data3$TRIP_SERIAL_NUMBER),as.character(data3$rte_points),as.character(data3$ROUTE_ABBR),sep = "_")
names(data3)[8]<-"nearestStormBearing"
write.table(data3,"D:\\traffic\\bus_weather_traffic_game_final.csv",sep=",",row.names = FALSE)

data4<-merge(x = data3, y = length_distance, by.x = c("TRIP_SERIAL_NUMBER","rte_points"),by.y = c("TRIP_ID","rte_points"), all.x = TRUE)

data5<-na.omit(data4)
sum(is.na(data4))

keeps<-c("uniqueid","startime","endtime","Game","Attendance","Start_game","End_game","day","Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","cloudCover","humidity",
         "nearestStormBearing","pressure","windSpeed","visibility", "windBearing","precipIntensity","precipType","hour","delay")

input_data<-data5[,c("uniqueid","date","startime","endtime","Start_game","End_game","day","hour","Game","precipType","Attendance","Free_Flow","SpeedLimit","Jamfactor","ozone","temperature","dewPoint","nearestStormDistance","cloudCover","humidity",
                     "nearestStormBearing","pressure","windSpeed","visibility", "windBearing","precipIntensity","LENGTH","TP1_NISSAN","TP2_NISSAN","delay","SCHEDULED_TRAVEL_TO","ACTUAL_TRAVEL_TO")]
input_data$Game<-as.factor(input_data$Game)
input_data$day<-as.factor(input_data$day)
input_data$precipType<-as.factor(input_data$precipType)

input_data<-input_data[,c("uniqueid","date","startime","endtime","Start_game","End_game","day","hour","Game","precipType","Attendance","Free_Flow","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","cloudCover","humidity",
  "nearestStormBearing","pressure","windSpeed","visibility", "windBearing","precipIntensity","LENGTH","TP1_NISSAN","TP2_NISSAN","SCHEDULED_TRAVEL_TO","ACTUAL_TRAVEL_TO")]

train_data<-input_data[(input_data$date < "2016-12-11")&(input_data$day==weekdays(as.Date("2016-12-11")))&(input_data$hour==11),c(9:23,26:30)]
validation_data<-input_data[((input_data$date == "2016-12-11") & (input_data$hour==11)),]
write.table(train_data,"D:\\traffic\\train_data.csv",sep=",",row.names = FALSE)
write.table(validation_data,"D:\\traffic\\validation_data.csv",sep=",",row.names = FALSE)

#===============================================================================

library(caret)
library(data.table)
# considering response variable as strata
set.seed(10501)
#data_partition <- createDataPartition(y = as.character(input_data$Game), p = 0.7, list = F)
dates<-as.Date(c("2016-11-23","2016-12-11","2016-12-18","2017-01-13","2017-01-26"))
test <- input_data[input_data$date %in% dates,] # 30% data goes here
train <- input_data[!(input_data$date %in% dates),] # 70% here
trainset<-data.table(train[,c("Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity","pressure","windSpeed","visibility", "precipIntensity","LENGTH","others","ACTUAL_TRAVEL_TO")])
testset<-data.table(test[,c("Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity","pressure","windSpeed","visibility", "precipIntensity","LENGTH","others","ACTUAL_TRAVEL_TO")])




strt<-Sys.time()
library(randomForest)
rf_game1=randomForest(formula = ACTUAL_TRAVEL_TO~., data=trainset,ntree=500, seed=1250)
pred_rf<-predict(rf_game1,test[,c("Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity","pressure","windSpeed","visibility", "precipIntensity","LENGTH","others")])

rmse_rf<-RMSE(test$ACTUAL_TRAVEL_TO,pred_rf$predictions)
rmse_rf
#rf_game2
print(Sys.time()-strt)

#===========cross validation of random forest====================
strt<-Sys.time()
library(e1071)
library(dplyr)
library(ranger)
rf_cvmodel<-train(ACTUAL_TRAVEL_TO~.,data=trainset,method="ranger",ntree=10,
                trControl=trainControl(method="cv",number=10),
                prox=TRUE,allowParallel=TRUE)
print(rf_cvmodel)

print(Sys.time()-strt)


strt<-Sys.time()
rf_game1=randomForest(x=as.matrix(trainset),y=train$ACTUAL_TRAVEL_TO,xtest=as.matrix(testset),ytest=test$ACTUAL_TRAVEL_TO,ntree=500,mtry=if (!is.null(y) && !is.factor(y))
  max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
  replace=TRUE, strata,
  #sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
  nodesize = 20,
  maxnodes = 250,importance=TRUE)
rf_game2=ranger(formula = ACTUAL_TRAVEL_TO~., data=train[,c("Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity","pressure","windSpeed","visibility", "precipIntensity","LENGTH","others","ACTUAL_TRAVEL_TO")],num.trees=500,mtry=8,importance ='impurity', seed=1250)
rf_game2
pred_ranger<-predict(rf_game2,test[,c("Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity","pressure","windSpeed","visibility", "precipIntensity","LENGTH","others","ACTUAL_TRAVEL_TO")])

rmse_ranger<-RMSE(test$ACTUAL_TRAVEL_TO,pred_ranger$predictions)
rmse_ranger
#rf_game2
print(Sys.time()-strt)

#some statistics
no_delays<-sum(input_data$ACTUAL_TRAVEL_TO>input_data$SCHEDULED_TRAVEL_TO)
no_delays
#histogram
attach(input_data)
#hist(ACTUAL_TRAVEL_TO, freq=FALSE, breaks=5,main="Density plot")
hist(ACTUAL_TRAVEL_TO, freq=FALSE, col="lightgreen", xlim=c(0,25),  ylim=c(0, .20))
curve(dnorm(x, mean=mean(ACTUAL_TRAVEL_TO), sd=sd(ACTUAL_TRAVEL_TO)), add=TRUE, col="darkblue", lwd=2)

hist(SCHEDULED_TRAVEL_TO, freq=FALSE, col="lightgreen", xlim=c(0,25),  ylim=c(0, .20))
curve(dnorm(x, mean=mean(SCHEDULED_TRAVEL_TO), sd=sd(SCHEDULED_TRAVEL_TO)), add=TRUE, col="darkblue", lwd=2)
detach(input_data)
train_data<-input_data[,c(5,6,10:25,27:30)]

library(data.table)
length_distance<-fread(file="D:\\traffic\\length_distances_km.csv",header=T,na.strings=c("","NA"))

library(ranger)
strt<-Sys.time()
rf_default=ranger(formula = delay~., train_data[,c(1:18)],num.trees=5000,importance ='impurity', seed=1250)
rf_default
print(Sys.time()-strt)



summary(rf_default)
var_imp<-importance(rf_default)


#================================================================================
trainset <- train_data[,c(-20,-22)]
trainout <-train_data[,]$ACTUAL_TRAVEL_TO
library(xgboost) 
  # xgboost fitting with arbitrary parameters
  xgb_params_2 = list(
    objective = "reg:linear",                                               # binary classification
    eta = 0.3,                                                                  # learning rate
    max.depth = 5,                                                               # max tree depth
    eval_metric = "rmse"                                                          # evaluation/loss metric
  )

# fit the model with the arbitrary parameters specified above
xgb_2 = xgboost(data = data.matrix(trainset),
                label= trainout,
                params = xgb_params_2,
                nrounds = 1000,                                                 # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 20,                                # stop if no improvement within 10 trees
                missing="NAN" 
)












# ===================== Prediction for Game day 2017-12-11=======================

#train_data<-input_data[(input_data$date < "2016-12-11")&(input_data$day==weekdays(as.Date("2016-12-11")))&(input_data$hour==11),c(1,5,6,8:28)]
#validation_data<-input_data[((input_data$date == "2016-12-11") & (input_data$hour==11)),c(1,5,6,8:28)]



library(ranger)
strt<-Sys.time()
rf_game1=ranger(formula = ACTUAL_TRAVEL_TO~., train_data[,c(1,2,5:20,22,23)],num.trees=5000,mtry=8,importance ='impurity', seed=1250)
rf_game1
print(Sys.time()-strt)






summary(rf_default)
var_imp<-importance(rf_default)

#===================================================================================





library(rfUtilities)
library(randomForest)

#start time
strt<-Sys.time()
rf_default1=randomForest(formula = ACTUAL_TRAVEL_TO~., data=train_data,ntree=100,maxnodes=250, seed=77)
rf_default1
print(Sys.time()-strt)
varImpPlot(rf_default1)
train_data<-input_data[,c("Free_Flow","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity",
                          "nearestStormBearing","pressure","windSpeed","visibility", "windBearing","precipIntensity","hour","SCHEDULED_TRAVEL_TO","ACTUAL_TRAVEL_TO")]

strt<-Sys.time()
rf2=randomForest(formula = ACTUAL_TRAVEL_TO~., data=train_data,ntree=10,maxnodes=250, seed=77)
rf2
print(Sys.time()-strt)
varImpPlot(rf2)

train_data<-input_data[,c("Free_Flow","SpeedLimit","temperature","dewPoint","nearestStormDistance","humidity",
                          "pressure","windSpeed","visibility","precipIntensity","hour","SCHEDULED_TRAVEL_TO","ACTUAL_TRAVEL_TO")]


strt<-Sys.time()
rf3=randomForest(formula = ACTUAL_TRAVEL_TO~., data=train_data,mtry=6,ntree=100,maxnodes=100, seed=77)
rf3
print(Sys.time()-strt)
varImpPlot(rf3)
x=input_data[,c("Game","Attendance","Free_Flow","SpeedLimit","temperature","dewPoint","nearestStormDistance","humidity",
                "pressure","windSpeed","visibility","precipIntensity","hour","SCHEDULED_TRAVEL_TO","ACTUAL_TRAVEL_TO")]
  tuneRF(x, y, mtryStart, ntreeTry=50, stepFactor=2, improve=0.05,
         trace=TRUE, plot=TRUE, doBest=FALSE, ...)






rf.mdl <- randomForest(input_res,input_data,maxnodes=5,ntree = 100)
(rf.cv <- rf.crossValidation(rf_default, input_data[,c(1:10,11)], p=0.10, n=10, ntree=501))
par(mfrow=c(2,2))
plot(rf.cv)
plot(rf.cv, stat = "mse")
plot(rf.cv, stat = "var.exp")
plot(rf.cv, stat = "mae")

library(caret)
InTrain<-createDataPartition(y=input_data$ACTUAL_TRAVEL_TO,p=0.3,list=FALSE)
training1<-input_data[InTrain,]


rf_model<-train(ACTUAL_TRAVEL_TO~.,data=input_data,method="ranger",
                trControl=trainControl(method="cv",number=2))
print(rf_model)











#======================================================================
library(parallel)
library(doParallel)
cl=makeCluster(3)
registerDoParallel(cl)
library(caret)
library(ggplot2)
library(ranger)
input_data1<-input_data[input_data$others!=1,]
library(caret)
library(data.table)
# considering response variable as strata
set.seed(10501)
#data_partition <- createDataPartition(y = as.character(input_data$Game), p = 0.7, list = F)
dates<-as.Date(c("2016-11-23","2016-12-11","2016-12-18","2017-01-13","2017-01-26"))
test <- input_data1[input_data1$date %in% dates,] # 30% data goes here
train <- input_data1[!(input_data1$date %in% dates),] # 70% here
trainset<-(train[,c("day","hour","Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity","pressure","windSpeed","visibility", "precipIntensity","LENGTH","ACTUAL_TRAVEL_TO")])
testset<-(test[,c("day","hour","Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity","pressure","windSpeed","visibility", "precipIntensity","LENGTH","ACTUAL_TRAVEL_TO")])

trainx<-data.matrix(train[,c("day","hour","Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity","pressure","windSpeed","visibility", "precipIntensity","LENGTH")])
testx<-(test[,c("day","hour","Free_Flow","Jamfactor","SpeedLimit","ozone","temperature","dewPoint","nearestStormDistance","humidity","pressure","windSpeed","visibility", "precipIntensity","LENGTH")])
trainy<-train$ACTUAL_TRAVEL_TO
testy<-test$ACTUAL_TRAVEL_TO
library(ranger)
rf1<-ranger(ACTUAL_TRAVEL_TO~.,data=trainset,num.trees =200,mtry=6,importance = "impurity",min.node.size = 100)

cctrl1 <- trainControl(method = "cv", number = 10, returnResamp = "all")
                       
test_class_rand <- train(trainx, trainy, 
                         method = "ranger", 
                         trControl = cctrl1,
                         tuneLength = 4,
                         num.trees = 200,
                         importance="impurity",
                         min.node.size=25,
                         seed = 345, 
                         num.threads = 3)
tes#rf1<-randomForest(ACTUAL_TRAVEL_TO~.,data=trainset,ntree=100,nodesize=25,mtry=6,importance=T)

#rf2<-rfcv(trainx, trainy, cv.fold=5, scale="log", step=0.5,mtry=function(p) max(1, (2*p/3)), recursive=FALSE,ntrees=100,nodesize=100)
library(caret)

detach(rf.bestmodel)
plot(rf.bestmodel$num.trees,rf.bestmodel$mtry)

data<-read.csv("D:\\traffic\\bus_weather_traffic_game_final.csv",sep=",")
