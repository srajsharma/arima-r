# 05-09-2017

# Calculation of Covariance and Correlation for store combinations
# Ranking them according to correlation
# Pulling net sales of test store and control stores at a week level 
# Indexing and calculating lower bound and upper bound

#--------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------- define library packages  ---------------------------------------------#

library("RMySQL")
library("sqldf")

# Function for calculating covariance and correlation

sales_cov_corr <- function(p1_agg_sales_table) 
{
  
  # Connection establishment
  library("RMySQL")
  newdrv <-dbDriver("MySQL");
  conn <- dbConnect(newdrv, dbname = 'masterdb',username = 'user', password = 'valoraWNS', host = '127.0.0.1', port = 3306)
  
  ## Output variable table name - change this to generate new table 
  cov_corr_table<- "ss_covcor_kk_food"
  
  print(p1_agg_sales_table)
  # Generate covarianve corr table only if not already generated
  if (dbExistsTable(conn, cov_corr_table)==FALSE){
    
    agg_sales_table <- p1_agg_sales_table
    
    # Store dimension
    dimension_store_table<- 'dim_store'
    
    # Fetching storeid and sum of netsales at a week level
    salesdf <- dbGetQuery(conn,paste0("select calendar_week, storeid, sum(netsales) as sales from masterdb.",agg_sales_table," group by calendar_week,storeid"));
    # Fetching Store information where stores are samestore
    storedf <- dbGetQuery(conn,paste0("select storeid,storesamestore from masterdb.",dimension_store_table," 
									  where storesamestore = 'Samestore'
									  and lower(trim(storeformat)) like 'k %'"));
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
      
      dataframe1<-unique(subset(salesdf, storeid==i, select = c(calendar_week,sales)))
      
      for(j in store_b) 
      {
        b<-b+1
        c<-c+1
        # i <- 10050
        # j <- 18393
        print(paste0(' Inner Loop : - ', c))
        
        if(a==1)
        {
          vardf<-paste0("df_",j)
          assign(vardf, as.data.frame(unique(subset(salesdf, storeid==j, select = c(calendar_week,sales)))))
        }
        tmpdf<-get( paste0("df_",j))
        join_res <- sqldf(paste0("select a.*, b.* from dataframe1 a inner join tmpdf b on a.calendar_week=b.calendar_week"))
        colnames(join_res) <- c("calendar_week" , "xsales", "calendar_week", "ysales")
        
        covar<-cov(join_res$xsales,join_res$ysales)
        corr<-cor(join_res$xsales,join_res$ysales)
        
        dataframenew <- data.frame(i,j,covar,corr)
        final_data <-rbind(final_data,dataframenew)
        print(paste0(' Total Count : - ', b))
      }
      # Eleminating first store after each iteration so that less number of iteraton can happen
      store_b<-sort(store_b[-1])
      # Batch Insert
      if(a%%5==0 || a==length(store_a))
      {
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
        
        # Reattaching with SQL
        library("RMySQL")
        
        newdrv <-dbDriver("MySQL");
        conn <- dbConnect(newdrv, dbname = 'masterdb',username = 'user', password = 'valoraWNS', host = '127.0.0.1', port = 3306)
        
        # Writting into database
        dbWriteTable(conn, value = final_data, name = cov_corr_table , row.names=F,append = TRUE)
        
        print(paste0("Records Added -- ",nrow(final_data)))
        final_data <- data.frame()
        
        # Disconnecting from database
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
  }
  return(cov_corr_table)
}




# Function to rank control stores according to corr value for a selected test store

TestControl_Diff <- function(p1_store, p2_n, p3_covcorr_weeksales)
{
  
  # Establishing connection
  drv <-dbDriver("MySQL");
  con1 <- dbConnect(drv, dbname = 'masterdb',username = 'user', password = 'valoraWNS', host = '127.0.0.1', port = 3306)
  
  store_id <- p1_store;
  n <- p2_n
  
  # Ranking control stores for the selected test store based on corr values 
  print(p3_covcorr_weeksales)
  covcorr_weeksales  <- p3_covcorr_weeksales
  cntrl_str <- paste0("select xstoreid, ystoreid, covar, corr from masterdb.",covcorr_weeksales," where xstoreid = ",store_id," and xstoreid <> ystoreid group by xstoreid, ystoreid, covar, corr order by corr desc") 
  cntrl_str_q <- paste0("select ystoreid from masterdb.",covcorr_weeksales," where xstoreid = ",store_id," and xstoreid <> ystoreid group by xstoreid, ystoreid, covar, corr order by corr desc limit 5") 
  rnk_cntrl_str<- dbGetQuery(con1, cntrl_str)
  # cntrl_str_ids<- dbGetQuery(con1, cntrl_str_q)
  
  # Ranking in desc order according to corr values
  rnk_cntrl_str$rank <- ave(-rnk_cntrl_str$corr, FUN = rank)
  rnk_cntrl_str
  
  
  # Pulling net sales of test store and control stores -- at a week level
  print(agg_sales_table)
  #agg_sales_table<-'rs_ts_Jan16_Jun17'
  netsales <- dbGetQuery(con1, paste0("select calendar_week, storeid, sum(netsales) as netsales from masterdb.",agg_sales_table," where calendar_year = '2016' group by calendar_week,storeid"));
  
  # Library Conflicts
  detach("package:RMySQL", unload=TRUE)
  
  n_cntrl_str <- sqldf(paste0("select rank, ystoreid from rnk_cntrl_str Where xstoreid = ",store_id, " AND rank <= ",n," group by rank,ystoreid"))
  inner_join1 <- merge(netsales,n_cntrl_str, by.x= 'storeid', by.y= 'ystoreid')
  weekly_rank <- sqldf("select calendar_week, rank, storeid, sum(netsales) as netsales from inner_join1 GROUP BY calendar_week, rank, storeid ORDER BY calendar_week, rank")
  
  # Reattaching RMySQL
  library("RMySQL")
  
  weekly_store_sales <- paste0("select calendar_week, storeid, sum(netsales) as netsales from masterdb.",agg_sales_table," where storeid = ", store_id ," group by calendar_week,storeid ")
  test_str_sale<- dbGetQuery(con1, weekly_store_sales)
  
  inner_join2 <- merge(test_str_sale,inner_join1, by = "calendar_week")
  colnames(inner_join2) <- c("calendar_week" , "xstoreid", "xnetsales", "ystoreid", "ynetsales", "rank")
  
  # Library conflicts
  detach("package:RMySQL", unload=TRUE)
  
  sales <- sqldf("select calendar_week, xstoreid, xnetsales as test_sales, rank, ystoreid, ynetsales as ctrl_sales from inner_join2 group by calendar_week, test_sales, rank, ctrl_sales")
  
  dstnct_teststore<-unique(sales$xstoreid)
  
  test_avg <- as.numeric(sqldf(paste0("select avg(a.test_sales) as ctrl_avg_sales from (select distinct xstoreid,calendar_week, test_sales from sales where xstoreid=",store_id,")as a ")))
  
  dstnct_ctrlstore<-unique(sales$ystoreid)
  
  dstct_n_ctrlstore <- sqldf(paste0("select avg(ctrl_sales) as ctrl_avg_sales, ystoreid from ( select distinct ystoreid,calendar_week, ctrl_sales from sales where ystoreid in ('",paste0(dstnct_ctrlstore,collapse = "','"),"') ) group by ystoreid"))
  
  # Indexing control sales
  indexing<-merge(sales,dstct_n_ctrlstore,by="ystoreid", all.x=TRUE)
  indexing$ctrl_index<-(indexing$ctrl_sales*test_avg)/indexing$ctrl_avg_sales
  
  # Calculating average (mu) and standard deviation (sigma) of indexed control sales
  avg_std <- sqldf("select calendar_week, xstoreid, test_sales, ystoreid, ctrl_sales, avg(ctrl_index) as mu, stdev(ctrl_index) as sigma from indexing group by calendar_week")
  
  # Calculating lower bound and upper bound
  ub <- avg_std$mu+1.645*avg_std$sigma
  lb <- avg_std$mu-1.645*avg_std$sigma
  
  ub_lb <- sqldf("select calendar_week, test_sales, mu, sigma, mu+1.645*sigma as UB, mu-1.645*sigma as LB from avg_std")
  
  # Writting into a csv file    
  write.csv(ub_lb, file = (paste0("Store_",store_id,"_",Sys.time(),".csv")))
  print(paste0('file generated at - ', getwd()))
  
}


# Attach RMySQL
library("RMySQL")

# Aggregated sales table at store-week level - Need to change table name if new sales weeks are required to be covered - schema should remain same
# This table is being generated using time series aggregation code
agg_sales_table<-'rs_ss_jan16_dec16_kk_food' #ss_ts_Jan15_dec16'

## sales_cov_corr function will return the cov-corr table for given aggregated sales
p3_covcorr_weeksales<-sales_cov_corr(agg_sales_table)

#############################################################################
#############################################################################

##Attach RMySQL
# library("RMySQL")
p3_covcorr_weeksales<- 'ss_covcor_Jan15_Dec16'

## TestControl_Diff will generate the ranking for a given store (p1) with number of control stores (p2) using table (p3)
## csv result set will be saved in given directory 
# Setting the path  
library("RMySQL")
path<-'/home/sharma'
setwd (path)
# TestControl_Diff(21012,5,p3_covcorr_weeksales)

