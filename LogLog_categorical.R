library(MASS)
library("RMySQL")
library("plyr")
drv <-dbDriver("MySQL");
con <- dbConnect(drv, dbname = 'samplewns',username = 'user', 
                 password = 'valoraWNS', host = '127.0.0.1', port = 3306)


# samplewns.ss_ag1_model / ss_ag2_model / ss_ag3_model / ss_ag4_model / ss_ag5_model
# rolluptable<-'ss_posrollup_salesforweather3'
# AG5 <- "Countlines","Mineralwasser","S_sswasser","Energy-Drinks","Bier","Sandwiches"
# AG4 <- "BonBons","Heissgetr_nke"
# AG3 <- "Schokolade","Zuckerwaren","Getr_nke","weisse Ware"
# AG2 <- "Food","Tabak"
# AG1 <- "Non Press"
#  AllAG  <- "Non Press","Food","Tabak","Schokolade","Zuckerwaren","Getr_nke","weisse Ware","BonBons",'Heissgetr_nke',"Countlines","Mineralwasser","S_sswasser","Energy-Drinks","Bier","Sandwiches"
# ss_ag1to5_model

jackpotdata<-'masterdb.jackp'
weathertable<-'samplewns.ss_allweathervariable_kelvin_zu'
# Setting up running model
model<-'linear' #log linear #log #linint


categortlst <-c("Mineralwasser")
modelhoursalestable<-'ss_zu_mineral_model_final'

for(category in categortlst){
  hrlist<- c(0,	1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	19,	20,	21,	22,	23)
  hrlist<-hrlist[8:24]
  plotdf<-data.frame()
  
  for(hr in hrlist){
    
    print(paste0("Category - ",category))
    salesdata <- dbGetQuery(con,paste0('select hr,DateDDMMYYYY,skeystore,sales 
                                       from samplewns.',modelhoursalestable,
                                       ' where hr = ',hr,' and Category like "',category,'%"'))
    
    unique_id<- data.frame(unique(salesdata$skeystore))
    city <- dbGetQuery(con, "SELECT * FROM  samplewns.ss_citynames1")
    weather <- dbGetQuery(con, paste0('SELECT * FROM  ',weathertable,' where hr = ',hr))
    jackpot<- dbGetQuery(con, paste0('SELECT * FROM  ',jackpotdata))
    
    salesdata_city <- merge(x = salesdata, y = city[ , c("skeystore", "city")], by = "skeystore", all.x=TRUE)
    salesdata_city_wather <- merge(x = salesdata_city, y = weather , by =c("DateDDMMYYYY", "city"))
    colnames(salesdata_city_wather)[4] <- "hr"
    salesdata_city_wather$hr.y <- NULL
    
    salesdata_city_wather$shiftedpd.f <- factor(salesdata_city_wather$shiftedpd)
    salesdata_city_wather$shiftedsn.f <- factor(salesdata_city_wather$shiftedsn)
    salesdata_city_wather$shiftedrr.f <- factor(salesdata_city_wather$shiftedrr)
   
    # is.factor(salesdata_city_wather$shiftedpd.f)
    #  levels(salesdata_city_wather$shiftedpd.f)
    # tapply(salesdata_city_wather$sales, salesdata_city_wather$shiftedpd.f, mean)
    # 
    # #assigning the treatment contrasts to shiftedpd.f
    # contrasts(salesdata_city_wather$shiftedpd.f) = contr.treatment(4)
    
    # declaring an output data frame 
    data_out<-data.frame()
    
    
    #loop for all the stores
    for(i in 1:nrow(unique_id))
    {
      a<-subset(salesdata_city_wather,salesdata_city_wather$skeystore==unique_id[i,], row.names=NULL)
      
      if((nrow(a) - sum(is.na(a$sales))) > 14){
        data_ts<-ts(a$sales,frequency = 7)
        data_decomstl<-stl(data_ts, t.window=730, s.window="periodic", robust=TRUE)
        # plot(data_decomstl)
        a$season<-data_decomstl$time.series[,"seasonal"]
        a$trend<-data_decomstl$time.series[,"trend"]
        a$random<-data_decomstl$time.series[,"remainder"]
        data_out<-rbind(data_out,a[1:nrow(a),])
      }else{
        print(paste0('Missing Store with less sale - ', unique_id[i,], ' with Count ', nrow(a), ' for hour ', hr ))
      }
    }
    
    
    if(nrow(data_out)>0){
      ## summation Season and Treand as independant variable -- baseline
      data_out$baseline <- data_out$season +  data_out$trend
      
      data_model<-data_out
      
      if(model == "log"){ 
       lmdata <- lm(logsales ~ logbaseline + logtemp1 + logPD + pd_cat, data = data_model)
      }
       if(model == "linear"){ 
         # TT + Tta + RH + RSD + PSD + CC + CL + RAD + RRAD + RRADD + UVI + PD + RR + FF + DD + PP + VV + SN
         # lmdata <- lm(sales ~ baseline + TT + shiftedsn.f + shiftedrr.f + shiftedpd.f , data = data_model)
         lmdata <- lm(sales ~ baseline + TT + Tta + RH + RSD + PSD + CC + CL + RAD + RRAD + RRADD + UVI + PD + RR + FF + DD + PP + VV + SN , data = data_model)
         step <- stepAIC(lmdata, direction="both")
         reducedmodel <- step$call
         lmdata <- eval(reducedmodel)
      }
      
      # step$anova # display results
      # cor(data_model$logtemp1,data_model$logsales)
      forexcel<-summary(lmdata)
      # nrow(forexcel$coefficients)
      zz<-data.frame()
      for(cnt in 1:nrow(forexcel$coefficients)){
        val<-rownames(forexcel$coefficients)[cnt]
        coff<- val
        var1<-paste0("stderr","_",val)
        var2<-paste0("tvalue","_",val)
        var3<-paste0("pvalue","_",val)
        var4<-paste0("sign","_",val,"_.001")
        
        assign(coff,(forexcel$coefficients)[cnt,1])
        assign(var1,(forexcel$coefficients)[cnt,2])
        assign(var2,(forexcel$coefficients)[cnt,3])
        assign(var3,(forexcel$coefficients)[cnt,4])
        # significance1.001 <- 
        assign(var4,if((forexcel$coefficients)[cnt,4]<.001)'Yes' else 'No')
        
        tmp <- data.frame( get(coff),get(var1),get(var2),get(var3),get(var4))
        colnames(tmp)[1] <- coff
        colnames(tmp)[2] <- var1
        colnames(tmp)[3] <- var2
        colnames(tmp)[4] <- var3
        colnames(tmp)[5] <- var4
        
          if(cnt==1){
            zz<-tmp
          }else{
            zz <-cbind(zz,tmp)
          }
      
        }
      
    }else{
      temp1<- 'NA'
      stderr2<- 'NA'
      tvalue2<- 'NA'
      pvalue2<-'NA'
      significance2.001 <- 'NA'
    }
    
    if(hr<13){
      hour<-paste0(hr,'AM')
    }
    else{
      hour<-paste0(hr-12,'PM')
    }
    
    zz<-cbind(hour,zz)
    zz$rsqre <- forexcel$r.squared
    zz$ad_rsqre <- forexcel$adj.r.squared
    zz$mse <- mean(forexcel$residuals^2)
    
    plotdf<-rbind.fill(plotdf,zz)
    
  }
  setwd('/home/sharma/STL_Model')
  filename<-paste0(category,'_',model,'_allweatherAIC_',Sys.time(),'.csv')
  # write.csv(file = filename,plotdf)
  
}
