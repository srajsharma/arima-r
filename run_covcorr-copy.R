library("RMySQL")
library("sqldf")
#library(hash)

# Function for calculating covariance and correlation

 
  # Connection establishment
  library("RMySQL")
  newdrv <-dbDriver("MySQL");
  conn <- dbConnect(newdrv, dbname = 'masterdb',username = 'user', password = 'valoraWNS', host = '127.0.0.1', port = 3306)
  covcorr_weeksales  <-'auto_covcorr_wksales'
   
    # Fethching data from sql database
    agg_sales_table<-'ss_ts_table'
    dimension_store_table<- 'dim_store'
    salesdf <- dbGetQuery(conn,paste0("select weekofyr, storeid, sum(netsales) as sales from masterdb.",agg_sales_table," group by weekofyr,storeid"));
    storedf <- dbGetQuery(conn, paste0("select storeid,storesamestore from masterdb.",dimension_store_table," where storesamestore = 'Samestore'"));
    
    # Library Conflicts
    detach("package:RMySQL", unload=TRUE)
    
    # Calculating covariance and correlation
    store_a <- sort(unique(storedf$storeid))     
    store_b<- sort(unique(storedf$storeid)) 
    final_data <- data.frame()
    a<-0
    b<-0
    
    # Geting current system time
    stime<-Sys.time()
    for( i in store_a) 
    {
      a<-a+1
      c<-0
      print(paste0(' Outer Loop : - ', a))
      
      dataframe1<-unique(subset(salesdf, storeid==i, select = c(weekofyr,sales)))
      
      for(j in store_b) 
      {
        #j<-store_b[jcount]
        b<-b+1
        c<-c+1
        # i <- 10050
        # j <- 18393
        print(paste0(' Inner Loop : - ', c))
        
        if(a==1){
          vardf<-paste0("df_",j)
          assign(vardf, as.data.frame(unique(subset(salesdf, storeid==j, select = c(weekofyr,sales)))))
        }
        #join_res <- merge(dataframe1, dataframe2, by ="weekofyr")
        tmpdf<-get( paste0("df_",j))
        join_res <- sqldf(paste0("select a.*, b.* from dataframe1 a inner join tmpdf b on a.weekofyr=b.weekofyr"))
        colnames(join_res) <- c("weekofyr" , "xsales", "weekofyr", "ysales")
        
        covar<-cov(join_res$xsales,join_res$ysales)
        corr<-cor(join_res$xsales,join_res$ysales)
        
        dataframenew <- data.frame(i,j,covar,corr)
        final_data <-rbind(final_data,dataframenew)
        print(paste0(' Total Count : - ', b))
      }
      store_b<-sort(store_b[-1])
      # print(paste0(' element remain : - ', length(store_b)))
      
      if(a%%5==0 || a==length(store_a)){
        # Adding transpose of data frame
        nrw<-nrow(final_data)
        upperdf <- data.frame(i= integer(nrw),j= integer(nrw),covar= double(nrw),corr= double(nrw))
        upperdf$i<-final_data$j
        upperdf$j<-final_data$i
        upperdf$covar<-final_data$covar
        upperdf$corr<-final_data$corr
        final_data <-unique(rbind(final_data,upperdf))
        # Changing column names of 'final_data'
        colnames(final_data) <- c("xstoreid" , "ystoreid", "covar", "corr")
        #tst<- as.data.frame(final_data[final_data$xstoreid==25888,])
        
        # Reattaching with SQL
        library("RMySQL")
        newdrv <-dbDriver("MySQL");
        conn <- dbConnect(newdrv, dbname = 'masterdb',username = 'user', password = 'valoraWNS', host = '127.0.0.1', port = 3306)
        
        # Writting into database
        dbWriteTable(conn, value = final_data, name = 'ss_covcorr_fast' , row.names=F,append = TRUE)
        print(paste0("Records Added -- ",nrow(final_data)))
        final_data <- data.frame()
        dbDisconnect(conn)
        conn<-NULL
        newdrv<-NULL
        # Library Conflicts
        detach("package:RMySQL", unload=TRUE)
        
      }
    }
  
    
    # Getting system time after loop runs
    etime<-Sys.time()
    
    # Calculating time difference
    etime-stime
 
