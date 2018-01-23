library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())
data<-read.csv(file="~\\weather_traffic5.csv",header=T,na.strings=c("","NA"))
data1<-read.csv(file="~\\weather_traffic25.csv",header=T,na.strings=c("","NA"))
data2<-read.csv(file="~\\weather_traffic4.csv",header=T,na.strings=c("","NA"))
tp_dis<-read.csv(file="~\\weather_traffic4.csv",header=T,na.strings=c("","NA"))

trips2<-data.table(data)
lg <- function(x)c(NA, x[1:(length(x)-1)])
trips2$timepoints<- ave(as.character(trips2$TIME_POINT_ABBR), as.character(trips2$ROUTE_ABBR), FUN=lg)
trips2$timepoints<- ave(as.character(trips2$TIME_POINT_ABBR), as.character(trips2$ROUTE_ABBR), FUN=lg)
trips2$rte_pnts<-paste(trips2$timepoints,as.character(trips2$TIME_POINT_ABBR),sep="-")
View(trips2)
trips2$rte_pnts<-paste(trips2$timepoints,as.character(trips2$TIME_POINT_ABBR),sep="-")
trips3<-trips2[!is.na(trips2$DELAY),]
trips3<-trips3[!is.na(trips3$schedule_dt_tm),]
trips3<-trips3[!is.na(trips3$actual_dt_tm),]
data<-trips3
trips2<-data.table(data1)
lg <- function(x)c(NA, x[1:(length(x)-1)])
trips2$timepoints<- ave(as.character(trips2$TIME_POINT_ABBR), as.character(trips2$ROUTE_ABBR), FUN=lg)
trips2$timepoints<- ave(as.character(trips2$TIME_POINT_ABBR), as.character(trips2$ROUTE_ABBR), FUN=lg)
trips2$rte_pnts<-paste(trips2$timepoints,as.character(trips2$TIME_POINT_ABBR),sep="-")
View(trips2)
View(trips2)
trips2$rte_pnts<-paste(trips2$timepoints,as.character(trips2$TIME_POINT_ABBR),sep="-")
trips3<-trips2[!is.na(trips2$DELAY),]
trips3<-trips2[!is.na(trips2$DELAY),]
trips3<-trips3[!is.na(trips3$schedule_dt_tm),]
trips3<-trips3[!is.na(trips3$actual_dt_tm),]
data1<-trips3
data5<-data1[,c("CALENDAR_ID","ROUTE_ABBR","ROUTE_DIRECTION_NAME","TRIP_SERIAL_NUMBER","TIME_POINT_ABBR","hour","day","timepoints","rte_pnts","CF.FF","CF.JF","CF.SU","ozone","temperature","dewPoint","nearestStormDistance" ,"humidity","nearestStormBearing","pressure" ,"windSpeed","visibility","windBearing","precipIntensity","precipType","SCHEDULED_TRAVEL_TO","ACTUAL_TRAVEL_TO","DELAY")]
names(data1=="CF.SP")<-"CF.SU"
names(data1== "CF.SP")<-"CF.SU"
[names(data1)== "CF.SP"]<-"CF.SU"
names(data1)
names(data1)[3]<-"CF.SU"
names(data)[3]<-"CF.SU"
data5<-data1[,c("CALENDAR_ID","ROUTE_ABBR","ROUTE_DIRECTION_NAME","TRIP_SERIAL_NUMBER","TIME_POINT_ABBR","hour","day","timepoints","rte_pnts","CF.FF","CF.JF","CF.SU","ozone","temperature","dewPoint","nearestStormDistance" ,"humidity","nearestStormBearing","pressure" ,"windSpeed","visibility","windBearing","precipIntensity","precipType","SCHEDULED_TRAVEL_TO","ACTUAL_TRAVEL_TO","DELAY")]
data3<-rbind(data,data1)
data3$CALENDAR_ID<-as.Date(data3$CALENDAR_ID)
data3$day<-weekdays(data3$CALENDAR_ID)
data3$hour<-format(as.POSIXct(trips1$actual_dt_tm,origin = "1970-01-01"),"%H")
data5<-data3[,c("CALENDAR_ID","ROUTE_ABBR","ROUTE_DIRECTION_NAME","TRIP_SERIAL_NUMBER","TIME_POINT_ABBR","hour","day","timepoints","rte_pnts","CF.FF","CF.JF","CF.SU","ozone","temperature","dewPoint","nearestStormDistance" ,"humidity","nearestStormBearing","pressure" ,"windSpeed","visibility","windBearing","precipIntensity","precipType","SCHEDULED_TRAVEL_TO","ACTUAL_TRAVEL_TO","DELAY")]
data3$hour<-format(as.POSIXct(trips1$actual_dt_tm,origin = "1970-01-01"),"%H")
data3$hour<-format(as.POSIXct(data3$actual_dt_tm,origin = "1970-01-01"),"%H")
data5<-data3[,c("CALENDAR_ID","ROUTE_ABBR","ROUTE_DIRECTION_NAME","TRIP_SERIAL_NUMBER","TIME_POINT_ABBR","hour","day","timepoints","rte_pnts","CF.FF","CF.JF","CF.SU","ozone","temperature","dewPoint","nearestStormDistance" ,"humidity","nearestStormBearing","pressure" ,"windSpeed","visibility","windBearing","precipIntensity","precipType","SCHEDULED_TRAVEL_TO","ACTUAL_TRAVEL_TO","DELAY")]
data<-rbind(data2,data5)
View(data5)
View(data)
data2$CALENDAR_ID<-as.Date(data2$CALENDAR_ID)
data<-rbind(data2,data5)
View(data)
View(data1)
write.table(data,"~\\final_data.csv",sep=",",row.names = FALSE)
data1<-data
data1<-data
data1<-data1[complete.cases(data1),]
data1$day<-as.factor(weekdays(as.Date(data1$CALENDAR_ID)))
data1$week <- as.factor(ifelse(data1$day %in% c("Saturday", "Sunday"), "weekend", "weekday"))
data2<-data1[(data1$SCHEDULED_TRAVEL_TO!=0|data1$ACTUAL_TRAVEL_TO!=0),]
View(data2)
View(data1)
game_dates<-read.csv(file="~\\Gamedata.csv",header=T,na.strings=c("","NA"))
tp_dis<-read.csv(file="~\\tp_distances.csv",header=T,na.strings=c("","NA"))

View(game_dates)
names(game_dates)[1]<-"CALENDAR_ID"
game_dates$CALENDAR_ID<-as.character(game_dates$CALENDAR_ID)
game_dates$CALENDAR_ID<-as.Date(game_dates$CALENDAR_ID,format="%d/%m/%Y")
game_dates$gameday<-NULL
game_dates$StartTime <- as.character(game_dates$StartTime)
game_dates$EndTime <- as.character(game_dates$EndTime)
data3<-merge(data2,game_dates,by.x="CALENDAR_ID",by.y="CALENDAR_ID",all.x = TRUE)
#data3$gameday[is.na(data3$gameday)]<-"NGD"
data3$StartTime[is.na(data3$StartTime)]<-"0:00"
data3$EndTime[is.na(data3$EndTime)]<-"0:00"
data3$Attendence[is.na(data3$Attendence)]<-0
data3$gameday<-NULL
data3$Gameday<-NULL
library(chron)
dat <- times(paste0(data3$StartTime, ":00"))
data3$StartTime<-hours(dat)
dat <- times(paste0(data3$EndTime, ":00"))
data3$EndTime<-hours(dat)


##################################################

names(tp_dis)[1]<-"TIME_POINT_ABBR"
tp_dis<-unique(tp_dis)
data4<-merge(data3,tp_dis,by.x="TIME_POINT_ABBR",by.y="TIME_POINT_ABBR",all.x = TRUE)
data4$LengthofGame<-NULL
write.table(data4,"~\\final_data14.csv",sep=",",row.names = FALSE)
