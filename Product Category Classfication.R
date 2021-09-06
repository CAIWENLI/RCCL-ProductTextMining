rm(list=ls())
library(RSiteCatalyst)
library(ggplot2)
library(RPostgreSQL)
library(dplyr)
library(dbplyr)
library(data.table)
library(lubridate)
library(reshape2)
library(stringr)
library(tidyr)
SCAuth("caiwenli@rccl.com:RCCI", "3e7a72d82de775fb55c31f4dcc51b4e0")

library(odbc)
con.microsoft.sql <- DBI::dbConnect(odbc::odbc(),
                                    Driver   = "SQL Server",
                                    Server   = "rcclanalyticssql.database.windows.net",
                                    Database = "rcclanalytics",
                                    UID      = "caiwenli",
                                    PWD      = "Travel.Florida2019!",
                                    Port     = 1433)

dbo.rev <- paste("select * from RCCLAnalytics.dbo.MIA_MobileBookingsRevMod t where t.PRODUCT_BOOKING_DT >= '2019-01-01' AND t.PRODUCT_BOOKING_DT < '2019-09-01'", sep="")


power.bi.sql  <- DBI::dbConnect(odbc::odbc(),
                                Driver   = "SQL Server",
                                Server   = "WINPOWERBI",
                                Database = "rcclanalytics",
                                UID      = "Tableau_User",
                                PWD      = "b8AEoTTwYH5sHFN6cuhF",
                                Port     = 1433)


dbo.rev.query <- dbGetQuery(con.microsoft.sql, dbo.rev)

tbl_vars(dbo.rev.query)

product.list.backend <- dbo.rev.query %>% 
  filter(CREATION_APPLICATION_NAME %in% "EXCALIBUR_MOBILE_APP") %>% 
  group_by(PRODUCT_NAME) %>% 
  summarize(revenue = sum(TOTAL_PAYMENT_AMT),
            orders = n())

product.list.adobe <- QueueRanked("rcciexcaliburprod",
                            "2019-01-01",
                            "2019-07-31",
                            c("revenue", "orders"),
                            c("evar115"),
                            top = 50000,
                            segment.id = "s300006910_59c41d59c06c5c10c89f449d")

productid.list.adobe <- QueueRanked("rcciexcaliburprod",
                                  "2019-01-01",
                                  "2019-07-31",
                                  c("revenue", "orders"),
                                  c("product"),
                                  top = 50000,
                                  segment.id = "s300006910_59c41d59c06c5c10c89f449d")

productcate.list.adobe <- QueueRanked("rcciexcaliburprod",
                                    "2019-01-01",
                                    "2019-07-31",
                                    c("revenue", "orders"),
                                    c("product"),
                                    top = 50000,
                                    segment.id = "s300006910_59c41d59c06c5c10c89f449d",
                                    classification = "Product Category")

library(stringr)
str_split_fixed(before$type, "_and_", 2)

productid.list.adobe.shorex <- productid.list.adobe %>% 
  select(name, revenue, orders) %>% 
  separate(name, c("pcp","product_cate", "number"), "-") %>% 
  group_by(product_cate) %>% 
  filter(!product_cate %in% "flexdining") %>% 
  summarize(revenue = sum(revenue))


productcate.list.backend <- dbo.rev.query %>% 
  filter(CREATION_APPLICATION_NAME %in% "EXCALIBUR_MOBILE_APP") %>% 
  group_by(OWNER_DESC) %>% 
  summarize(revenue = sum(TOTAL_PAYMENT_AMT)) 
productcate.list.backend$OWNER_DESC[1] ="dining"
productcate.list.backend$OWNER_DESC[2]= "entertainment"
productcate.list.backend$OWNER_DESC[3] ="shorex"


proudct.cate.rev <- productcate.list.backend %>% 
  full_join(productid.list.adobe.shorex, by = c("OWNER_DESC" = "product_cate")) 

colnames(proudct.cate.rev) <- c("product.cate", "backend.rev", "adobe.rev")

product.cate.rev <- proudct.cate.rev %>% 
  mutate(diff = adobe.rev - backend.rev,
         diff_per = round((adobe.rev - backend.rev)/backend.rev,4))
