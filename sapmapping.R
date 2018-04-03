library("RMySQL")
library("openxlsx")
library("data.table")

path<-'/data/data.marketing'
setwd (path)
list.files()
mydf <- read.xlsx(xlsxFile  = "Contentplan App.xlsx", sheet = 2, startRow = 8,colNames = FALSE)

obs <- nrow(mydf)
print(paste0("observation : ", obs))

drv <-dbDriver("MySQL");
con <- dbConnect(drv, dbname = 'masterdb',username = 'admin', 
                 password = 'valoraWNS', host = '127.0.0.1', port = 3306)
dbGetQuery(con,paste0("TRUNCATE TABLE ", "auto_sap_lb_campmapping"))
for (i in  1 : obs){
  #i<-45
  
  row <- mydf[i,]
  exl<-i+7
  campname<-enc2utf8(as.character(row$X1)) 
  campname <- iconv(campname, from = "UTF-8", to = "LATIN1")
  zrt<-enc2utf8(as.character(row$X11))
  zrt <- iconv(zrt, from = "UTF-8", to = "LATIN1")
  
  fst<-as.character(strsplit(as.character(row$X20), ",")[[1]] )
  scnd<-as.character(strsplit(as.character(row$X21), ",")[[1]] )
  trd<-as.character(strsplit(as.character(row$X22), ",")[[1]] )
  frth<-as.character(strsplit(as.character(row$X23), ",")[[1]] )
  ffth<-as.character(strsplit(as.character(row$X24), ",")[[1]] )
  sxth<-as.character(strsplit(as.character(row$X25), ",")[[1]] )
  svn<-as.character(strsplit(as.character(row$X26), ",")[[1]] )
  
  eth<-if(is.na(as.character(row$X27))) 0 else 1
  nth<-as.character(row$X28)
  tnt<-as.character(row$X29)
  elvn<-as.character(row$X30)

  if(!is.na(campname) && campname!='Fishermans - Staffel-Deal; erste Runde 2.50/2.00/1.50 etc. '){
  joinset <- CJ(campname=campname ,Start_month=zrt,SAP_CampaignID=fst,LB_CampaignID=scnd,LB_ScheduleID=trd,
                SAP_campID_Freestampcard_Prod=frth,LB_campID_Freestampcard_Prod=ffth,LB_scheduleID_Freestampcard_Prod=sxth,
                coupon_related_to_stampcard=svn,special_campaign_flag=eth,start_wk=nth,end_wk=tnt,Campaign_type=elvn)
  
  }else if (!is.na(campname) && campname=='Fishermans - Staffel-Deal; erste Runde 2.50/2.00/1.50 etc. '){
    # special case
    joinset <- CJ(campname=campname ,Start_month=zrt,
                          SAP_campID_Freestampcard_Prod=frth,LB_campID_Freestampcard_Prod=ffth,LB_scheduleID_Freestampcard_Prod=sxth,
                          coupon_related_to_stampcard=svn,special_campaign_flag=eth,start_wk=nth,end_wk=tnt,Campaign_type=elvn)
    joinset <- data.frame(cbind(joinset,SAP_CampaignID=fst,LB_CampaignID=scnd))
    
    joinset <- merge(joinset,data.frame(LB_ScheduleID = trd),all = TRUE)
    
    joinset <- joinset[c("campname","Start_month","SAP_CampaignID","LB_CampaignID","LB_ScheduleID",
              "SAP_campID_Freestampcard_Prod","LB_campID_Freestampcard_Prod","LB_scheduleID_Freestampcard_Prod",
              "coupon_related_to_stampcard","special_campaign_flag","start_wk","end_wk","Campaign_type")]
  }
  
  if(exists("joinset") && !is.na(campname) && campname != " - "){
  joinset[is.na(joinset)] = "NA"
  
  print(paste0("Excel Row : ", exl, " for records : ", nrow(joinset)))
  
  
  dbWriteTable(con, value = joinset, name = "auto_sap_lb_campmapping" , row.names=F,append = TRUE) 
  rm(joinset)
  }
}

Sys.time()
#quit(save = 'no')
