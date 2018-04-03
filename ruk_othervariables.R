#===================================================================#
# LOG-LOG MODEL:  EFFECT OF DIFFERENT TEMPERATURE VARIABLES ON SALES #
#===================================================================#

#-----------------#
# to check/change #
#-----------------#

# --- category to run for (check in relevant table and give input)
# --- hours to run for
# --- table names (and other data sources) and queries
# --- database you're connecting to 
# --- directory you're writing the o/p file to

#----------------------#
# establish connection #
#----------------------#

library("RMySQL")
drv <-dbDriver("MySQL");
con <- dbConnect(drv, dbname = 'samplewns',username = 'user', 
                 password = 'valoraWNS', host = '127.0.0.1', port = 3306)

#-------------#
# table names #
#-------------#

# samplewns.ss_ag1_model / ss_ag2_model / ss_ag3_model / ss_ag4_model / ss_ag5_model

# rolluptable<-'ss_posrollup_salesforweather3'
# AG5 <- "Countlines","Mineralwasser","S_sswasser","Energy-Drinks","Bier","Sandwiches".....
# AG4 <- "BonBons"....
# AG3 <- "Schokolade","Zuckerwaren","Getr_nke","weisse Ware"....
# AG2 <- "Food","Tabak".....
# AG1 <- "Non Press"

#-----------------#
# jackpot dataset #
#-----------------#

jackpotdata<-'masterdb.jackp'

#-----------------#
# weather dataset #
#-----------------#

weathertable<-'samplewns.ss_allweathervariable'

#---------------------------------------------------------------------#
# CHECK/CHANGE: desired categories to run for and relevant table name #
#---------------------------------------------------------------------#

# category list
categortlst <-c("12391")

# check/change master table
modelhoursalestable<-'ss_ag5_model'

# define output data frame
plot_data<-data.frame()


#----------------------------------------#
# model - looping for category and hours #
#----------------------------------------#

# outer loop - category
for(category in categortlst){
  
  # hour list
  hrlist<- c(0,         1,            2,            3,            4,            5,            6,            7,            8,            9,            10,          11,          12,          13,                14,          15,          16,          17,          18,          19,          20,          21,          22,          23)
  
  # Only required hours
  hrlist<-hrlist[8:12]
  
  # defining output data frame
  plotdf<-data.frame()
  
  # inner loop - desired hours
  for(hr in hrlist){
    
    # selected category
    print(paste0("Category - ",category))
    
    # get sales data for the selected category for the selected hours -- from the master table selected above
    salesdata <- dbGetQuery(con,paste0('select hr,DateDDMMYYYY,skeystore,sum(sumsales) as sales from samplewns.ss_posrollup_salesforweather3', ' where hr = ',hr,
                                       ' and articleid = "',category,'" 
                                       group by 1,2,3'))
    
    # get uinque storeIDs
    unique_id<- data.frame(unique(salesdata$skeystore))
    
    # get city data 
    city <- dbGetQuery(con, "SELECT * FROM  samplewns.gb_citynames1")
    
    # get weather data
    weather <- dbGetQuery(con, paste0('SELECT * FROM  ',weathertable,' where hr = ',hr))
    
    # get jackpot data
    jackpot<- dbGetQuery(con, paste0('SELECT * FROM  ',jackpotdata))
    
    # merge city data to sales data 
    salesdata_city <- merge(x = salesdata, y = city[ , c("skeystore", "city")], by = "skeystore", all.x=TRUE)
    
    # merge weather data to the above
    salesdata_city_wather <- merge(x = salesdata_city, y = weather[ , c("DateDDMMYYYY", "shiftedtt", "PD", "RR", "city")], by =c("DateDDMMYYYY", "city"))
    
    # declaring an output data frame 
    data_out<-data.frame()
    
    # checking for NAs
    as<-salesdata_city_wather[is.na(salesdata_city_wather$shiftedtt),]
    
    #loop for all the stores
    for(i in 1:nrow(unique_id))
    {
      # subset data for the selected store
      a<-subset(salesdata_city_wather,salesdata_city_wather$skeystore==unique_id[i,], row.names=NULL)
      
      # data sufficiency condition check
      if(nrow(a)>14){
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
      
      data_model$logsales <- log(data_out$sales)
      data_model$logbaseline <- log(data_out$baseline)
      ## Negative number handling
      # Y = {-3,1,2,.,5,10,100};    /* negative datum */
      # LY = log10(Y + 1 - min(Y)); /* translate, then transform */
      
      #data_model$logtemp1 <- log(data_out$TT + 1 - min(data_out$TT))
      
      data_model$logtemp1 <- log(data_out$shiftedtt)
      data_model$logtemp2 <- log(data_out$PD)
      data_model$logtemp3 <- log(data_out$RR)
      #data_model$logtemp2 <- log(data_out$Tta + 1 - min(data_out$Tta))
      data_model$lograndom <- log(data_out$random)
      
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
      
      data_model$logtemp3[which(is.nan(data_model$logtemp3))] = NA
      data_model$logtemp3[which(data_model$logtemp3==Inf)] = NA
      data_model$logtemp3[which(!is.finite(data_model$logtemp3))] = NA
      
      lmdata <- lm(logsales ~ logbaseline + logtemp1 + logtemp2 + logtemp3, data = data_model)
      
      # cor(data_model$logtemp1,data_model$logsales)
      forexcel<-summary(lmdata)
      
      intercept<-forexcel$coefficients[1,1]
      stderr0<- forexcel$coefficients[1,2]
      tvalue0<- forexcel$coefficients[1,3]
      pvalue0<- forexcel$coefficients[1,4]
      significance0.001 <- if(pvalue0<.001)'Yes' else 'No'
      
      baseline<-forexcel$coefficients[2,1]
      stderr1<- forexcel$coefficients[2,2]
      tvalue1<- forexcel$coefficients[2,3]
      pvalue1<- forexcel$coefficients[2,4]
      significance1.001 <- if(pvalue1<.001)'Yes' else 'No'
      
      temp1<-forexcel$coefficients["logtemp1","Estimate"]
      stderr2<- forexcel$coefficients["logtemp1","Std. Error"]
      tvalue2<- forexcel$coefficients["logtemp1","t value"]
      pvalue2<- forexcel$coefficients["logtemp1","Pr(>|t|)"]
      significance2.001 <- if(pvalue2<.001)'Yes' else 'No'
      
      temp2<-forexcel$coefficients["logtemp2","Estimate"]
      stderr3<- forexcel$coefficients["logtemp2","Std. Error"]
      tvalue3<- forexcel$coefficients["logtemp2","t value"]
      pvalue3<- forexcel$coefficients["logtemp2","Pr(>|t|)"]
      significance3.001 <- if(pvalue3<.001)'Yes' else 'No'
      
      temp3<-forexcel$coefficients["logtemp3","Estimate"]
      stderr4<- forexcel$coefficients["logtemp3","Std. Error"]
      tvalue4<- forexcel$coefficients["logtemp3","t value"]
      pvalue4<- forexcel$coefficients["logtemp3","Pr(>|t|)"]
      significance4.001 <- if(pvalue4<.001)'Yes' else 'No'
      
      
    }else{
      temp1<- 'NA'
      stderr2<- 'NA'
      tvalue2<- 'NA'
      pvalue2<-'NA'
      significance2.001 <- 'NA'
      
      temp2<- 'NA'
      stderr3<- 'NA'
      tvalue3<- 'NA'
      pvalue3<-'NA'
      significance3.001 <- 'NA'
      
      temp3<- 'NA'
      stderr4<- 'NA'
      tvalue4<- 'NA'
      pvalue4<-'NA'
      significance4.001 <- 'NA'
      
    }
    
    if(hr<13){
      hour<-paste0(hr,'AM')
    }
    else{
      hour<-paste0(hr-12,'PM')
    }
    
    a<-data.frame(hour, 
                  intercept, stderr0, tvalue0, pvalue0, significance0.001,
                  baseline, stderr1, tvalue1, pvalue1, significance1.001,
                  temp1, stderr2, tvalue2, pvalue2, significance2.001,
                  temp2, stderr3, tvalue3, pvalue3, significance3.001,
                  temp3, stderr4, tvalue4, pvalue4, significance4.001)
    
    plotdf<-rbind(plotdf,a)
    
    # exlfile<-paste0('Model Results_',category,'.xlsx')
    # sheetname<-hour
    # write.xlsx(x=forexcel,file = exlfile)
  }
  setwd('/home/sunil/STLModels')
  filename<-paste0(category,'_Temp_',Sys.time(),'.csv')
  # write.csv(file = filename,plotdf)
}
