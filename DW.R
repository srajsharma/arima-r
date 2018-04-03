library("RMySQL")
library("splines")
drv <-dbDriver("MySQL");
con <- dbConnect(drv, dbname = 'masterdb',username = 'user', 
                 password = 'valoraWNS', host = '127.0.0.1', port = 3306)

covcortable <- 'masterdb.ss_covcor_kk_food'
dbGetQuery(con,'drop table if exists ss_rank')
unique_id <- dbGetQuery(con,paste0('select distinct xstoreid from ',covcortable))
plotdf<-data.frame()
for(i in 1:nrow(unique_id))
{
  xstore<-unique_id[i,]
  # select * from masterdb.rs_covcor_Jan16_Dec16 where xstoreid='14881' and ystoreid!='14881' order by corr desc limit 1;
  
  
  topone <- dbGetQuery(con,paste0('select * from ',covcortable,' where xstoreid=',xstore,' and ystoreid != ',xstore,'  and corr is not NULL order by corr desc'))
  if(nrow(topone)>0){
  ystore<-topone$ystoreid
  corr<-topone$corr
  a<-data.frame(xstore,ystore,corr)
  dbWriteTable(con, value = a, name = 'ss_rank', row.names=T,append = TRUE ) 
  }
  # plotdf<-rbind(plotdf,a)  
}

setwd('/home/sharma')
filename<-paste0('top5rank_',Sys.time(),'.csv')
# write.csv(file = filename,plotdf)
