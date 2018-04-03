library("RMySQL")
drv <-dbDriver("MySQL");
con <- dbConnect(drv, dbname = 'samplewns',username = 'user', 
                 password = 'valoraWNS', host = '127.0.0.1', port = 3306)
category<-'Lotto'
rolluptable<-'ss_posrollup_salesforweather1'
'ss_Lose_hrchck'
'srvc_excl_lott'
storehrtable<-paste0('ss_',category,'_hrchck')
'ss_ts_Lose_allhr'
storedayhrtable <- paste0('ss_ts_',category,'_allhr')
jackpotdata<-'masterdb.jackp'
hrlist<- c(0,	1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	19,	20,	21,	22,	23)
## Only Required
hrlist<-hrlist[8:24]
plotdf<-data.frame()
jackpot<- dbGetQuery(con, paste0('SELECT * FROM  ',jackpotdata))

for(hr in hrlist){
  # hr<-hrlist[12]
  # paste0('select skeystore from ss_Lose_hrchck where days > 40 and hr = ',hr)
  
  salesdata <- dbGetQuery(con,paste0('select hr,DateDDMMYYYY,skeystore,sales from samplewns.',storedayhrtable,' where skeystore in (select skeystore from samplewns.',storehrtable,' where days > 40 and hr = ',hr, ') and hr = ',hr))
  unique_id<- data.frame(unique(salesdata$skeystore))
  city <- dbGetQuery(con, "SELECT * FROM  samplewns.gb_citynames1")
  weather <- dbGetQuery(con, paste0('SELECT * FROM  samplewns.gb_',hr,'hrtemp'))
  
  salesdata_city <- merge(x = salesdata, y = city[ , c("skeystore", "city")], by = "skeystore", all.x=TRUE)
  salesdata_city_wather <- merge(x = salesdata_city, y = weather[ , c("DateDDMMYYYY", "TT", "Tta", "city")], by =c("DateDDMMYYYY", "city"))
  salesdata_city_wather_jp <- merge(x = salesdata_city_wather, y = jackpot[ , c("DateDDMMYYYY", "Jackpot_Swiss_Lotto")], by =c("DateDDMMYYYY"))
 
  data_out<-data.frame()
  # min(salesdata_city_wather$TT)
  # as<-salesdata_city_wather[is.na(salesdata_city_wather$TT),]
  for(i in 1:nrow(unique_id))
  {
    # i <- 1
    a<-subset(salesdata_city_wather_jp,salesdata_city_wather_jp$skeystore==unique_id[i,], row.names=NULL)
    if(nrow(a)>=7){
      data_ts<-ts(a$sales,frequency = 7)
      data_decomstl<-stl(data_ts, t.window=730, s.window="periodic", robust=TRUE)
      a$season<-data_decomstl$time.series[,"seasonal"]
      a$trend<-data_decomstl$time.series[,"trend"]
      a$random<-data_decomstl$time.series[,"remainder"]
      data_out<-rbind(data_out,a[1:nrow(a),])
    }else{
      print(paste0('Missing Store with less sale - ', unique_id[i,], ' with Count ', nrow(a), ' for hour ', hr ))
    }
  }
  
  if(nrow(data_out)>0){
  ## summation Season and Treand as independant variable
  data_out$baseline <- data_out$season +  data_out$trend
  
  data_model<-data_out
  
  data_model$logsales <- log(data_out$sales)
  data_model$logbaseline <- log(data_out$baseline)
  ## Negative number handling
  # Y = {-3,1,2,.,5,10,100};    /* negative datum */
  # LY = log10(Y + 1 - min(Y)); /* translate, then transform */
  data_model$logtemp1 <- log(data_out$TT + 1 - min(data_out$TT))
  data_model$logtemp2 <- log(data_out$Tta + 1 - min(data_out$Tta))
  data_model$lograndom <- log(data_out$random)
  data_model$logJackpot <- log(data_out$Jackpot_Swiss_Lotto)
  
  data_model$logsales[which(is.nan(data_model$logsales))] = NA
  data_model$logsales[which(data_model$logsales==Inf)] = NA
  data_model$logsales[which(!is.finite(data_model$logsales))] = NA
  
  data_model$logbaseline[which(is.nan(data_model$logbaseline))] = NA
  data_model$logbaseline[which(data_model$logbaseline==Inf)] = NA
  data_model$logbaseline[which(!is.finite(data_model$logbaseline))] = NA
  
  
  data_model$logtemp1[which(is.nan(data_model$logtemp1))] = NA
  data_model$logtemp1[which(data_model$logtemp1==Inf)] = NA
  data_model$logtemp1[which(!is.finite(data_model$logtemp1))] = NA
  
  
  data_model$logtemp2[which(is.nan(data_model$logtemp2))] = NA
  data_model$logtemp2[which(data_model$logtemp2==Inf)] = NA
  data_model$logtemp2[which(!is.finite(data_model$logtemp2))] = NA
  
  lmdata <- lm(logsales ~ logbaseline + logtemp1 + logJackpot, data = data_model)
  
  # cor(data_model$logtemp1,data_model$logsales)
  forexcel<-summary(lmdata)
  
  temp<-forexcel$coefficients["logtemp1","Estimate"]
  stderr<- forexcel$coefficients["logtemp1","Std. Error"]
  tvalue<- forexcel$coefficients["logtemp1","t value"]
  pvalue<- forexcel$coefficients["logtemp1","Pr(>|t|)"]
  significance.001 <- if(pvalue<.001)'Yes' else 'No'
  
  jpcoff<-forexcel$coefficients[4,1]
  stderr2<- forexcel$coefficients[4,2]
  tvalue2<- forexcel$coefficients[4,3]
  pvalue2<- forexcel$coefficients[4,4]
  significance2.001 <- if(pvalue2<.001)'Yes' else 'No'
  
}else{
  temp<- 'NA'
  stderr<- 'NA'
  tvalue<- 'NA'
  pvalue<-'NA'
  significance.001 <- 'NA'
}

if(hr<13){
  hour<-paste0(hr,'AM')
}
else{
  hour<-paste0(hr-12,'PM')
}

a<-data.frame(hour,temp,jpcoff,stderr,tvalue,pvalue,significance.001,stderr2,tvalue2,pvalue2,significance2.001)
plotdf<-rbind(plotdf,a)

# exlfile<-paste0('Model Results_',category,'.xlsx')
# sheetname<-hour
# write.xlsx(x=forexcel,file = exlfile)
}
setwd('/home/sharma/STL_Model')
filename<-paste0(category,'_',Sys.time(),'.csv')
write.csv(file = filename,plotdf)
