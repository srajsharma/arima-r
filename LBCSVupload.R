library("tools")
library("RMySQL")
library("xlsx")
library("openxlsx")
# detach("package:xlsx", unload=TRUE)

drv <-dbDriver("MySQL");
con <- dbConnect(drv, dbname = 'masterdb',username = 'admin', password = 'valoraWNS', host = '127.0.0.1', port = 3306)

rm(mydf)
rm(dataset)
dbListTables(con)



## For Liquid-Barcodes-kkiosk-app.csv
path <-'/data/data.liquid-barcodes'
setwd (path)
tname<-"fact_liquidbarcodes"
dataset <- read.table("Liquid-Barcodes-kkiosk-app.csv", header=1, sep=",", quote="\"", dec=".", na.strings=c("NA", "\"\""), allowEscapes=1, fill=0);
dbGetQuery(con,paste0("TRUNCATE TABLE ", tname))
dbWriteTable(con, value = dataset, name = tname, row.names=F,append = TRUE ) 

## For LB_registration_data.csv
path <-'/data/data.liquid-barcodes'
setwd (path)
dataset <- read.table("LB_registration_data.csv", header=1, sep=",", quote="", dec=".", na.strings=c("NA", "\"\""), allowEscapes=1, fill=0);
dbGetQuery(con,paste0("TRUNCATE TABLE ", "dim_LBregistration"))
dbWriteTable(con, value = dataset, name = "dim_LBregistration", row.names=F,append = TRUE )
    
## For spettacolo
path<-'/data/data.spettacolo'
setwd (path)
list.files()
options(java.parameters = "-Xmx4048m")
# xlsxmydf <-read.xlsx(file = "2017-11-01 ordered_items-Details.xlsx",sheetIndex = 1,startRow = 1,colIndex = 1,endRow = 33873)
mydf <- read.xlsx(xlsxFile  = "2017-11-01 ordered_items-Details-SS.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
dbWriteTable(con, value = mydf, name = "orderedFull", row.names=F,append = TRUE ) 

## For Store Export
path <- '/home/sharma'
setwd(path)
dataset <- read.table('store_export.txt', header=TRUE, sep="\t", fill = TRUE,
                      colClasses="character",fileEncoding="latin1",quote="")
dbGetQuery(con,paste0("TRUNCATE TABLE ", "dim_store_locator"))
dbWriteTable(con, value = dataset, name = "dim_store_locator", row.names=F,append = TRUE ) 

mydf <- read.xlsx(xlsxFile  = "Copy of Storelist_TS.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
dbWriteTable(con, value = mydf, name = "Storelist_TS", row.names=F,append = TRUE ) 


## For camp_article_mapping.csv
path <-'/data/data.liquid-barcodes'
setwd (path)
dataset <- read.table("camp_article_mapping.csv", header=1, sep=",", quote="", dec=".", na.strings=c("NA", "\"\""), allowEscapes=1, fill=0);
dbGetQuery(con,paste0("TRUNCATE TABLE ", "dim_camparticlemapping"))
dbWriteTable(con, value = dataset, name = "dim_camparticlemapping", row.names=F,append = TRUE )

## For other mapping files
path <- '/data/data.teradata.wns'
setwd (path)
mydf <- read.xlsx(xlsxFile  = "campaign_mapping_v5.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
dbWriteTable(con, value = mydf, name = "dim_sap_lb_campmapping", row.names=F,append = TRUE ) 

mydf <- read.xlsx(xlsxFile  = "am_dim_discount_v3.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
dbWriteTable(con, value = mydf, name = "am_dim_discount", row.names=F,append = TRUE ) 

mydf <-  read.csv("JP_2016.csv");
dbWriteTable(con, value = mydf, name = "fact_Jackpot2", row.names=F,append = TRUE ) 



