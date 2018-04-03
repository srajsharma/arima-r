rm(list=ls())
library("RMySQL")
drv <-dbDriver("MySQL");
con <- dbConnect(drv, dbname = 'samplewns',username = 'user', 
                 password = 'valoraWNS', host = '127.0.0.1', port = 3306)

# rolluptable<-'ss_posrollup_salesforweather3'
# AG5 <- "Countlines","Mineralwasser","S_sswasser","Energy-Drinks","Bier","Sandwiches"
# AG4 <- "BonBons"
# AG3 <- "Schokolade","Zuckerwaren","Getr_nke","weisse Ware"
# AG2 <- "Food","Tabak"
# AG1 <- "Non Press"

jackpotdata<-'masterdb.jackp'
weathertable<-'samplewns.ss_allweathervariable_kelvin_zu' #_kelvin _kelvin_zu _zu

categortlst <- c("Non Press")
modelhoursalestable<-'ss_ag1_model_final'

plot_data<-data.frame()
model<-'linear' #log linear #log #linint

for(category in categortlst){
  
  hrlist<- c(0,	1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	19,	20,	21,	22,	23)
  ## Only Required
  ## category<-"Food"
  hrlist<-hrlist[8:24]
  plotdf<-data.frame()
  data_final<- data.frame()
  
  for(hr in hrlist){
    # hr<-7
    # category <- "Mineralwasser"
    print(paste0("Category - ",category," for hour ",hr))
    salesdata <- dbGetQuery(con,paste0('select hr,DateDDMMYYYY, DateDDMMYYYY as currdate,skeystore,sales from samplewns.',modelhoursalestable,' where hr = ',hr,' and Category like "',category,'%"'))
    newsalesdata <- dbGetQuery(con,paste0('select hr, DateDDMMYYYY, DATE_SUB(DateDDMMYYYY , interval 1 day) as currdate ,skeystore,sales as nxthrsales from samplewns.',modelhoursalestable,' where hr = ',hr,' and Category like "',category,'%"'))
    unique_id<- data.frame(unique(salesdata$skeystore))
    
    city <- dbGetQuery(con, "SELECT * FROM  samplewns.ss_citynames1")
    weather <- dbGetQuery(con, paste0('SELECT * FROM  ',weathertable,' where hr = ',hr))
    
    jackpot<- dbGetQuery(con, paste0('SELECT * FROM  ',jackpotdata))
    
    salesdata_city1 <- merge(x = salesdata, y = city[ , c("skeystore", "city")], by = "skeystore", all.x=TRUE)
    salesdata_city2 <- merge(x = newsalesdata, y = city[ , c("skeystore", "city")], by = "skeystore", all.x=TRUE)
    
    salesdata_city<- merge(x= salesdata_city1,  y = salesdata_city2, by = c("currdate", "skeystore", "city"))[ ,c("DateDDMMYYYY.x", "hr.x", "skeystore", "city","sales","nxthrsales")]
    colnames(salesdata_city)[1] <- "DateDDMMYYYY"
    colnames(salesdata_city)[2] <- "hr"
    salesdata_city_wather <- merge(x = salesdata_city, y = weather[ , c("DateDDMMYYYY", "TT", "Tta","shiftedtt", "city")], by =c("DateDDMMYYYY", "city"))
    salesdata_city_wather_jp <- merge(x = salesdata_city_wather, y = jackpot[ , c("DateDDMMYYYY", "Jackpot_Swiss_Lotto","Jackpot_Euro_Millions")], by =c("DateDDMMYYYY"))
    
    data_out<-data.frame()
    
    if(nrow(salesdata_city_wather_jp)>=50){
      ## summation Season and Treand as independant variable
      # data_out$baseline <- data_out$season +  data_out$trend
      
      data_model<-salesdata_city_wather_jp
      
      if(model=="linear"){
        print(model)
        # lmdata <- lm(sales ~ TT + Jackpot_Swiss_Lotto + Jackpot_Euro_Millions, data = data_model)
        # Sales(t) = Intercept + [B] * Baseline(t-1) + [C] * Baseline(t-1) * Temp(t-1) where Baseline := Sales
          lmdata <- lm(nxthrsales ~  sales + sales*TT, data = data_model)
      }
      
      if(model=="linint"){
        print(model)
        # lmdata <- lm(sales ~ tt*Jackpot_Swiss_Lotto + tt*Jackpot_Euro_Millions, data = data_model)
      }
      
      if(model=="log"){
        print(model)
        lmdata <- lm(logsales ~ logtemp1 + logJackpot_sl + logJackpot_em, data = data_model)
      }
      
      # cor(data_model$logtemp1,data_model$logsales)
      forexcel<-summary(lmdata)
      
      intercept<-forexcel$coefficients[1,1]
      stderr0<- forexcel$coefficients[1,2]
      tvalue0<- forexcel$coefficients[1,3]
      pvalue0<- forexcel$coefficients[1,4]
      significance0.001 <- if(pvalue0<.001)'Yes' else 'No'
      
      sales<-forexcel$coefficients[2,"Estimate"]
      stderr1<- forexcel$coefficients[2,"Std. Error"]
      tvalue1 <- forexcel$coefficients[2,"t value"]
      pvalue1 <- forexcel$coefficients[2,"Pr(>|t|)"]
      significance1.001 <- if(pvalue1<.001)'Yes' else 'No'
      
      sales_temp<-forexcel$coefficients[3,1]
      stderr2<- forexcel$coefficients[3,2]
      tvalue2<- forexcel$coefficients[3,3]
      pvalue2<- forexcel$coefficients[3,4]
      significance2.001 <- if(pvalue2<.001)'Yes' else 'No'
      
      rsqre <- forexcel$r.squared
      ad_rsqre <- forexcel$adj.r.squared
      MSE <- mean(forexcel$residuals^2)
      
      predictset <-  predict(lmdata, data_model, interval="predict")
      data_model <- cbind(data_model,predictset[1:nrow(predictset),])
      data_final <- rbind(data_final,data_model[1:nrow(data_model),])
      
    }else{
      intercept<- 'NA'   ;  stderr0<- 'NA'   ;  tvalue0<- 'NA'   ;  pvalue0<- 'NA'   ;  significance0.001<- 'NA'   ;  sales<- 'NA'   ;  stderr1<- 'NA'   ;  tvalue1<- 'NA'   ;  pvalue1<- 'NA'   ;  significance1.001<- 'NA'   ;  sales_temp<- 'NA'   ;  stderr2<- 'NA'   ;  tvalue2<- 'NA'   ;  pvalue2<- 'NA'   ;  significance2.001<- 'NA'   ;  rsqre<- 'NA'   ;  ad_rsqre<- 'NA'   ;  MSE <- 'NA'
    }
    
    
    # hour<-paste0(hr+1,' hour')
    if(hr<13){
      hour<-paste0(hr,'AM')
    }
    else{
      hour<-paste0(hr-12,'PM')
    }
    
    a <-data.frame(hour, 
                   intercept, stderr0, tvalue0, pvalue0, significance0.001,
                   sales, stderr1, tvalue1, pvalue1, significance1.001,
                   sales_temp, stderr2, tvalue2, pvalue2, significance2.001,
                   rsqre, ad_rsqre, MSE)
    
    plotdf<-rbind(plotdf,a)
    
  }
  
  setwd('/home/sharma/STL_Model')
  if(grepl("^[[:digit:]]",category)){
    category <- dbGetQuery(con,paste0('select article from masterdb.dim_articles where articleid = ',category, ' limit 1'))
  }
  filename<-paste0(category,'_allciti_',model,'_AR1_',Sys.time(),'.csv')
  write.csv(file = filename,plotdf)
  tablename<-paste0('ss_allciti_',category,'_',model)
  # dbGetQuery(con,paste0('drop table if exists ',tablename))
  # dbWriteTable(con, value = data_final, name = tablename,row.names=F,append = FALSE)
  print(paste0('file created - ',filename))
  
}