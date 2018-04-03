library("RMySQL")
drv <-dbDriver("MySQL");
con <- dbConnect(drv, dbname = 'samplewns',username = 'user', 
                 password = 'valoraWNS', host = '127.0.0.1', port = 3306)

path <- '/data/data.teradata.wns'
setwd (path)

gb_19hr_Hotdrinks <- dbGetQuery(con, "SELECT * FROM  samplewns.gb_ts_Hotdrinks19hr")
gb_19hr_Hotdrinks
gb_strlist_Hotdrinks <- dbGetQuery(con, "SELECT * FROM samplewns.gb_strlist19hr_Hotdrinks")
gb_strlist_Hotdrinks

gb_city <- dbGetQuery(con, "SELECT * FROM  samplewns.gb_citynames1")
gb_weather <- dbGetQuery(con, "SELECT * FROM  samplewns.gb_19hrtemp")
gb_city
gb_weather

output19hr_Hotdrinks_1 <- merge(x = gb_19hr_Hotdrinks, y = gb_city[ , c("skeystore", "city")], by = "skeystore", all.x=TRUE)

output19hr_Hotdrinks_1

output19hr_Hotdrinks_2 <- merge(x = output19hr_Hotdrinks_1, y = gb_weather[ , c("DateDDMMYYYY", "TT", "Tta", "city")], by =c("DateDDMMYYYY", "city"), all.x=TRUE)

output19hr_Hotdrinks_2
data_out<-data.frame()
unique_id<-data.frame(unique(gb_strlist_Hotdrinks$skeystore))

unique_id
gb_strlist_Hotdrinks


for(i in 1:nrow(unique_id))
{
  print(i)
  a<-subset(output19hr_Hotdrinks_2,output19hr_Hotdrinks_2$skeystore==unique_id[i,], row.names=NULL)
  data_ts<-ts(a$sales,frequency = 7)
  data_decomstl<-stl(data_ts, t.window=730, s.window="periodic", robust=TRUE)
  a$season<-data_decomstl$time.series[,"seasonal"]
  a$trend<-data_decomstl$time.series[,"trend"]
  a$random<-data_decomstl$time.series[,"remainder"]
  data_out<-rbind(data_out,a[1:nrow(a),])
  
}

output19hr_Hotdrinks <- data_out

output19hr_Hotdrinks$baseline <- output19hr_Hotdrinks$season +  output19hr_Hotdrinks$trend
output19hr_Hotdrinks

str(output19hr_Hotdrinks)



output19hr_Hotdrinks$logsales <- log(output19hr_Hotdrinks$sales)

output19hr_Hotdrinks$logbaseline <- log(output19hr_Hotdrinks$baseline)

output19hr_Hotdrinks$logtemp1 <- log(output19hr_Hotdrinks$TT)

output19hr_Hotdrinks$logtemp2 <- log(output19hr_Hotdrinks$Tta)

output19hr_Hotdrinks$lograndom <- log(output19hr_Hotdrinks$random)

output19hr_Hotdrinks$logsales[which(is.nan(output19hr_Hotdrinks$logsales))] = NA
output19hr_Hotdrinks$logsales[which(output19hr_Hotdrinks$logsales==Inf)] = NA
output19hr_Hotdrinks$logsales[which(!is.finite(output19hr_Hotdrinks$logsales))] = NA

output19hr_Hotdrinks$logbaseline[which(is.nan(output19hr_Hotdrinks$logbaseline))] = NA
output19hr_Hotdrinks$logbaseline[which(output19hr_Hotdrinks$logbaseline==Inf)] = NA
output19hr_Hotdrinks$logbaseline[which(!is.finite(output19hr_Hotdrinks$logbaseline))] = NA


output19hr_Hotdrinks$logtemp1[which(is.nan(output19hr_Hotdrinks$logtemp1))] = NA
output19hr_Hotdrinks$logtemp1[which(output19hr_Hotdrinks$logtemp1==Inf)] = NA
output19hr_Hotdrinks$logtemp1[which(!is.finite(output19hr_Hotdrinks$logtemp1))] = NA


output19hr_Hotdrinks$logtemp2[which(is.nan(output19hr_Hotdrinks$logtemp2))] = NA
output19hr_Hotdrinks$logtemp2[which(output19hr_Hotdrinks$logtemp2==Inf)] = NA
output19hr_Hotdrinks$logtemp2[which(!is.finite(output19hr_Hotdrinks$logtemp2))] = NA

lmoutput19hr_Hotdrinks_2 <- lm(logsales ~ logbaseline + logtemp1, data = output19hr_Hotdrinks)

summary(lmoutput19hr_Hotdrinks_2)

lmoutput19hr_Hotdrinks_2_2 <- lm(logsales ~ logbaseline + logtemp2, data = output19hr_Hotdrinks_2)

summary(lmoutput19hr_Hotdrinks_2_2)

lmoutput19hr_Hotdrinks_2_4 <- lm(lograndom ~ logtemp1, data = output19hr_Hotdrinks_2)

summary(lmoutput19hr_Hotdrinks_2_4)

lmoutput19hr_Hotdrinks_2_5 <- lm(lograndom ~ logtemp2, data = output19hr_Hotdrinks_2)

summary(lmoutput19hr_Hotdrinks_2_5)



