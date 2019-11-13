library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plyr)
library(leaps)
library(rio)
library(foreign)

###
setwd("~/Desktop")
homesales <- readRDS("phoenix_data_grantee_frequency.rds")
homesales <- separate(homesales, col= "SALE DATE (MMYYYY)",into = c('SALE MONTH', 'SALE YEAR'), sep = -4, convert=TRUE)
### add lat and longs to original homesales dataset
homesales <- as.data.table(homesales)
homesales$`GRANTEE OWNER NAME`[(homesales$`GRANTEE OWNER NAME` == "BEAZER HOMES SALES  INC.")] = "BEAZER HOMES SALES INC" ### fixing spelling error
homesales <- homesales[(homesales$`SALE YEAR` >= 2009),] #only for 2005+
names(homesales)[1] <- "parcel_id" ## rename to parcelid
### add merge lat and longs 
homesales <- homesales[,c(1,3,4,10,11,23,30,31,32,33,34,37,38,39,40,46)]
homesales <- homesales[fulllatlong, on = ("parcel_id"="parcel_id")]
####
homesales <- homesales[which(!is.na(homesales[,6])==TRUE),] ## removing rows with no owner
homesales <- homesales[which(!is.na(homesales[,17])==TRUE),] ## removing rows with no long lat
#saveRDS(homesales, file="homesales_w_longlat.rds")
homesales <- readRDS("homesales_w_longlat.rds")
timetable <- readRDS("Phoenix_Owners_Timetable_w_LatLong.rds")
timetable <- timetable[timetable$YEAR>2005,]

#
### spatial joining ### spatial joining ### spatial joining ### spatial joining
#
  
library(rgdal)
library(mapview)
library(sf)
library(dplyr)
library(rgdal)
library(mapview)
## set working directory to attendancezoneboundaries in Dropbox
setwd("~/Dropbox/attendanceZoneBoundaries/SABS_1516_SchoolLevels")
schoolbounds <- st_read('SABS_1516_Primary.shp') ## loading boundaries of primary schools
schoolbounds<- schoolbounds[which(schoolbounds$stAbbrev == 'AZ'),] ## only arizona primary schools
length(unique(schoolbounds$schnam))
proj_schools <- st_crs(schoolbounds)
###
districts_location <- '~/Dropbox/attendanceZoneBoundaries/sales_shapes.rds'
### parcels with lat and long
homesales <- st_as_sf(homesales, coords = c("long", "lat"), crs = 4326)
homesales <- st_transform(homesales, proj_schools)
#saveRDS(homesales, districts_location) ## saves transformed homesales with lat long in metric form to location
### matches schools to homesale parcel
homesales <- st_join(homesales, schoolbounds['schnam'], join = st_intersects)
homesales <- as.data.table(homesales)
homesales <- homesales[,-c("PROPERTY TYPE OTHER DESCRIPTION","geometry","FINANCE TYPE OTHER DESCRIPTION", "PARTIAL INTEREST PERCENT", "NUMBER OF PARCELS", "BUY/SELL RELATIONSHIP")]
###saving data table
setwd("~/Desktop")
#saveRDS(homesales, file="Homesales_w_Schools.rds") # save table
### REPEAT SPATIAL JOIN FOR TIMETABLE FOR OWNERSHIP
timetable <-  st_as_sf(timetable , coords = c("long", "lat"), crs = 4326)
timetable  <- st_transform(timetable , proj_schools)
#matches schools to TIMETABLE
timetable  <- st_join(timetable , schoolbounds['schnam'], join = st_intersects)
timetable  <- as.data.table(timetable)
timetable <- timetable[,-c("geometry","First_Sale")]
#saveRDS(timetable, file="Timetable_w_Schools.rds") # save table

#
### spatial joining ### spatial joining ### spatial joining ### spatial joining
#

#### TIMETABLE TO FIND INVESTORS by WHO OWNED PARCEL IN 2015
timetable <- readRDS("Timetable_w_Schools.rds")

##### counting owners by year
for (i in 2006:2015) {
  timetable[(timetable$YEAR==i), PROPERTIES_OWNED_THIS_YEAR:=.N, by= OWNER]
}
### RANKING OWNERS BY NUMBER OF PROPERTIES OWNED BY YEAR
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR >= 1000), SIZE:='MAX', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR < 1000) , SIZE:='1000', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR < 500) , SIZE:='500', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR <100), SIZE:='100', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR <50), SIZE:='50', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR <10), SIZE:='10', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR == 4), SIZE:='4', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR == 3), SIZE:='3', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR == 2), SIZE:='2', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR == 1), SIZE:='1', by=PROPERTIES_OWNED_THIS_YEAR]
#saveRDS(timetable, file="timetable_final.rds")


timetable <- readRDS("timetable_final.rds")
timetable15 <- timetable[timetable$YEAR==2015,]
### GET LIST OF INVESTORS; FIND PROPORTION OF INVESTORS 
all_investor_names <- timetable15[SIZE %in% c("3","4","10","50","100","500","1000","MAX")]
all_investor_names <- unique(all_investor_names$OWNER)
all_investor_names <- all_investor_names[all_investor_names!='']
## change to homesales if looking only for sales, not ownership
small_investor_names <- timetable15[SIZE %in% c("3","4","10")]#between 3 and 9 properties
small_investor_names <- unique(small_investor_names[,OWNER]) 
small_investor_names <- small_investor_names[!small_investor_names=='']
medium_investor_names <- timetable15[SIZE %in% c("50","100")]#between 10 and 99 properties
medium_investor_names <- unique(medium_investor_names[,OWNER]) 
medium_investor_names <- medium_investor_names[!medium_investor_names=='']
large_investor_names <- timetable15[SIZE %in% c("500","1000","MAX")]#more than 100 properties
large_investor_names <- unique(large_investor_names[,OWNER]) 
large_investor_names <- large_investor_names[!large_investor_names=='']

# proportion of investors by years HOMESALES
homesales <- readRDS("Homesales_w_Schools.rds")
homesales <- homesales[, investor_purchase:=0]
homesales <- homesales[, small_investor_purchase:=0]
homesales <- homesales[, medium_investor_purchase:=0]
homesales <- homesales[, large_investor_purchase:=0]
homesales <- homesales[homesales$`GRANTEE OWNER NAME` %in% all_investor_names, investor_purchase:=1]
homesales <- homesales[homesales$`GRANTEE OWNER NAME` %in% small_investor_names, small_investor_purchase:=1]
homesales <- homesales[homesales$`GRANTEE OWNER NAME` %in% medium_investor_names, medium_investor_purchase:=1]
homesales <- homesales[homesales$`GRANTEE OWNER NAME` %in% large_investor_names, large_investor_purchase:=1]

# proportion of investors by years TIMETABLE
timetable <- timetable[, investor_purchase:=0]
timetable <- timetable[, small_investor_purchase:=0]
timetable <- timetable[, medium_investor_purchase:=0]
timetable <- timetable[, large_investor_purchase:=0]
timetable <- timetable[timetable$OWNER %in% all_investor_names, investor_purchase:=1]
timetable <- timetable[timetable$OWNER %in% small_investor_names, small_investor_purchase:=1]
timetable <- timetable[timetable$OWNER %in% medium_investor_names, medium_investor_purchase:=1]
timetable <- timetable[timetable$OWNER %in% large_investor_names, large_investor_purchase:=1]

#### share of investors PURCHASING HOMES by school HOMESALES
dt <- data.table(int_year=homesales$`SALE YEAR`,
                 n_sales_year=homesales$freq.grantee, 
                 school=homesales$schnam,
                 buyer=homesales$`GRANTEE OWNER NAME`)
dt <- dt[, investor_purchase:=0]
dt_all_investor_names <- data.table(name = all_investor_names)
dt <- dt[buyer %in% all_investor_names, investor_purchase:=1]
dt <- na.omit(dt)
#investor purchases by schools
dt_table <- table(dt[, .(int_year, investor_purchase, school)])
dt_table <- data.table(dt_table)
dt_table <- dcast(dt_table, int_year + school ~investor_purchase, value.var="N")
dt_table <- dt_table[, tot_sales:=`0`+`1`]
dt_table$tot_sales[dt_table$tot_sales==0] <- 0.000000001 #changing to non zero for division
dt_table <- dt_table[, share_investor:=`1`/tot_sales]
dt_table$share_investor <- dt_table$share_investor*100 #changing to full percentages
dt_table$int_year <- as.integer(dt_table$int_year) 
dt_table <- dt_table[,-c(3,4,5)]
dt_table <- dt_table[dt_table$int_year>2009,]
## average across all data for purchases
dt_table_ave <- table(dt[, .(int_year, investor_purchase)])
dt_table_ave <- data.table(dt_table_ave)
dt_table_ave <- dcast(dt_table_ave, int_year ~investor_purchase, value.var="N")
dt_table_ave <- dt_table_ave[, tot_sales:=`0`+`1`]

dt_table_ave$tot_sales[dt_table_ave$tot_sales==0] <- 0.000000001 #changing to non zero for division
dt_table_ave <- dt_table_ave[, share_investor:=`1`/tot_sales]
dt_table_ave$share_investor <- dt_table_ave$share_investor*100
dt_table_ave$int_year <- as.integer(dt_table_ave$int_year) 
dt_table_ave <- dt_table_ave[,-c(2,3,4)]
dt_table_ave <- dt_table_ave[dt_table_ave$int_year>2009,]
#### share of investors OWNING HOMES by school TIMETABLE
dt_own <- data.table(int_year=timetable$YEAR, 
                     n_sales_year=timetable$PROPERTIES_OWNED_THIS_YEAR, 
                     school=timetable$schnam,
                     owner=timetable$OWNER)
dt_own <- dt_own[, investor_purchase:=0]
dt_all_investor_names <- data.table(name = all_investor_names)
dt_own <- dt_own[owner %in% all_investor_names, investor_purchase:=1]
dt_own <- na.omit(dt_own)
dt_table_own <- table(dt_own[, .(int_year, investor_purchase, school)])  # add school
dt_table_own <- data.table(dt_table_own)
dt_table_own <- dcast(dt_table_own, int_year + school ~ investor_purchase, value.var="N") # remove school to left side for plots
dt_table_own <- dt_table_own[, tot_sales:=`0`+`1`]
dt_table_own$tot_sales[dt_table_own$tot_sales==0] <- 0.000000001 #changing to non zero for division
dt_table_own <- dt_table_own[, share_investor:=`1`/tot_sales]
dt_table_own$share_investor <- dt_table_own$share_investor*100
dt_table_own$int_year <- as.integer(dt_table_own$int_year) 
names(dt_table_own)[6] <- "share_investor_owned"
dt_table_own <- dt_table_own[dt_table_own$int_year>2009,]
dt_table_own <- dt_table_own[,-c(3,4,5)]
### average investor ownership
dt_table_ave_own <- table(dt_own[, .(int_year, investor_purchase)])  
dt_table_ave_own <- data.table(dt_table_ave_own)
dt_table_ave_own <- dcast(dt_table_ave_own, int_year ~ investor_purchase, value.var="N") 
dt_table_ave_own <- dt_table_ave_own[, tot_sales:=`0`+`1`]
dt_table_ave_own$tot_sales[dt_table_ave_own$tot_sales==0] <- 0.000000001 #changing to non zero for division
dt_table_ave_own <- dt_table_ave_own[, share_investor:=`1`/tot_sales]
dt_table_ave_own$share_investor <- dt_table_ave_own$share_investor*100
dt_table_ave_own$int_year <- as.integer(dt_table_ave_own$int_year) 
names(dt_table_ave_own)[5] <- "share_investor_owned"
dt_table_ave_own <- dt_table_ave_own[dt_table_ave_own$int_year>2009,]
dt_table_ave_own <- dt_table_ave_own[,-c(2,3,4)]
###
###

##### plot growing shares of investor purchases
ggplot(data=dt_table_ave, aes(x=int_year, y=share_investor, group=1)) + 
  geom_line(col="dodgerblue2") + geom_point()
##### plot growing shares of investor ownership
ggplot(data=dt_table_ave_own, aes(x=int_year, y=share_investor_owned, group=1)) + 
  geom_line(col="red") + geom_point()
### adding column for share of investors in district
