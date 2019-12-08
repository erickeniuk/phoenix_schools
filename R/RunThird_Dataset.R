library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plyr)
library(leaps)
library(rio)
library(foreign)
library(wesanderson)
library(scales)
library(RNOmni)

homesales <- readRDS("homesales_w_longlat.rds")
timetable <- readRDS("Phoenix_Owners_Timetable_w_LatLong.rds") ## roughly 7.8 million obs if correct, 778k parcels
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
length(unique(schoolbounds$schnam)) # 807 unique schools
proj_schools <- st_crs(schoolbounds)
### parcels with lat and long
homesales <- st_as_sf(homesales, coords = c("long", "lat"), crs = 4326)
homesales <- st_transform(homesales, proj_schools)
### matches schools to homesale parcel
homesales <- st_join(homesales, schoolbounds['schnam'], join = st_intersects)
homesales <- as.data.table(homesales)
homesales <- homesales[,-c("PROPERTY TYPE OTHER DESCRIPTION","geometry","FINANCE TYPE OTHER DESCRIPTION", "PARTIAL INTEREST PERCENT", "NUMBER OF PARCELS", "BUY/SELL RELATIONSHIP")]
## if warning, ignore, okay. Just means column was already removed
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
## if warning, ignore, okay. Just means column was already removed
#saveRDS(timetable, file="Timetable_w_Schools.rds") # save table 17,431,660 observations
#
### SPATIAL JOIN COMPLETE; SCHOOLS ADDED
#
timetable <- readRDS("Timetable_w_Schools.rds")
##### counting owners by year
for (i in 2005:2015) {
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
####
####          START FROM HERE FOR FINAL RUN                 START FROM HERE FOR FINAL RUN
####
timetable <- readRDS("timetable_final.rds") # 17,431,660 observations if correct
timetable15 <- timetable[timetable$YEAR==2015]
### GET LIST OF INVESTORS; FIND PROPORTION OF INVESTORS 
all_investor_names <- timetable15[SIZE %in% c("3","4","10","50","100","500","1000","MAX")]
all_investor_names <- all_investor_names[,c(2,3,5)]
all_investor_names <- unique(all_investor_names$OWNER)
all_investor_names <- all_investor_names[all_investor_names!='']
## change to homesales if looking only for sales, not ownership
### FIX FIX FIX FIX ^^^^ TO MATCH
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
homesales <- homesales[homesales$`SALE YEAR`>=2005,]

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
####
###median home sale value by year by school
dtprice <- data.table(int_year=homesales$`SALE YEAR`,
                 price=homesales$`SALE PRICE`, 
                 school=homesales$schnam,
                 buyer=homesales$`GRANTEE OWNER NAME`)
medprice <- dtprice[,.(med_price=median(price)),by=list(int_year,school)]
#investor purchases by schools by years
# home PURCHASES
#### share of investors PURCHASING HOMES by school HOMESALES
dt <- data.table(int_year=homesales$`SALE YEAR`,
                 n_sales_year=homesales$freq.grantee, 
                 school=homesales$schnam,
                 buyer=homesales$`GRANTEE OWNER NAME`)
dt <- dt[, investor_purchase:=0]
dt <- dt[buyer %in% all_investor_names, investor_purchase:=1]
#investor purchases by schools
dt_table <- dt[,.(sum_investors=sum(investor_purchase),homes_purchased=.N),by=list(int_year,school)]
dt_table <- dt_table[,share_investor_purchases:=100*(sum_investors/homes_purchased),]
dt_table <- dt_table[,-c(3,4)]
## average across all data for purchases by year
dt_table_ave <- dt[,.(sum_investors=sum(investor_purchase),homes_purchased=.N),by=list(int_year)]
dt_table_ave <- dt_table_ave[,share_investor_purchases:=100*(sum_investors/homes_purchased),]
dt_table_ave <- dt_table_ave[,-c(2,3)]
### small investor purchases
dtsmall <- data.table(int_year=homesales$`SALE YEAR`,
                 n_sales_year=homesales$freq.grantee, 
                 school=homesales$schnam,
                 buyer=homesales$`GRANTEE OWNER NAME`)
dtsmall <- dtsmall[, investor_purchase:=0]
dtsmall <- dtsmall[buyer %in% small_investor_names, investor_purchase:=1]
#investor purchases by schools
dt_tablesmall <- dtsmall[,.(sum_investors=sum(investor_purchase),homes_purchased=.N),by=list(int_year,school)]
dt_tablesmall <- dt_tablesmall[,share_small_investor_purchases:=100*(dt_tablesmall$sum_investors/dt_tablesmall$homes_purchased),]
dt_tablesmall <- dt_tablesmall[,-c(3,4)]
## average across all data for purchases by year
dt_table_avesmall <- dtsmall[,.(sum_investors=sum(investor_purchase),homes_purchased=.N),by=list(int_year)]
dt_table_avesmall <- dt_table_avesmall[,share_small_investor_purchases:=100*(dt_table_avesmall$sum_investors/dt_table_avesmall$homes_purchased),]
dt_table_avesmall <- dt_table_avesmall[,-c(2,3)]
### medium investor purchases
dt <- data.table(int_year=homesales$`SALE YEAR`,
                 n_sales_year=homesales$freq.grantee, 
                 school=homesales$schnam,
                 buyer=homesales$`GRANTEE OWNER NAME`)
dt <- dt[, investor_purchase:=0]
dt <- dt[buyer %in% medium_investor_names, investor_purchase:=1]
#investor purchases by schools
dt_tablemed <- dt[,.(sum_investors=sum(investor_purchase),homes_purchased=.N),by=list(int_year,school)]
dt_tablemed <- dt_tablemed[,share_medium_investor_purchases:=100*(sum_investors/homes_purchased),]
dt_tablemed <- dt_tablemed[,-c(3,4)]
## average across all data for purchases by year
dt_table_avemed <- dt[,.(sum_investors=sum(investor_purchase),homes_purchased=.N),by=list(int_year)]
dt_table_avemed <- dt_table_avemed[,share_medium_investor_purchases:=100*(sum_investors/homes_purchased),]
dt_table_avemed <- dt_table_avemed[,-c(2,3)]

### LARGE investor purchases
dt <- data.table(int_year=homesales$`SALE YEAR`,
                 n_sales_year=homesales$freq.grantee, 
                 school=homesales$schnam,
                 buyer=homesales$`GRANTEE OWNER NAME`)
dt <- dt[, investor_purchase:=0]
dt <- dt[buyer %in% large_investor_names, investor_purchase:=1]
#investor purchases by schools
dt_tablelarge <- dt[,.(sum_investors=sum(investor_purchase),homes_purchased=.N),by=list(int_year,school)]
dt_tablelarge <- dt_tablelarge[,share_large_investor_purchases:=100*(sum_investors/homes_purchased),]
dt_tablelarge <- dt_tablelarge[,-c(3,4)]
## average across all data for purchases by year
dt_table_avelarge <- dt[,.(sum_investors=sum(investor_purchase),homes_purchased=.N),by=list(int_year)]
dt_table_avelarge <- dt_table_avelarge[,share_large_investor_purchases:=100*(sum_investors/homes_purchased),]
dt_table_avelarge <- dt_table_avelarge[,-c(2,3)]

### OWNERSHIP
#### share of investors OWNING HOMES by school TIMETABLE
### OWNERSHIP
dtown <- data.table(int_year=timetable$YEAR, 
                    n_sales_year=timetable$PROPERTIES_OWNED_THIS_YEAR, 
                    school=timetable$schnam,
                    owner=timetable$OWNER)
dtown <- dtown[, investor_purchase:=0]
dtown <- dtown[owner %in% all_investor_names, investor_purchase:=1]
#investor purchases by schools
dt_tableown <- dtown[,.(sum_investors=sum(investor_purchase),homes_owned=.N),by=list(int_year,school)]
dt_tableown <- dt_tableown[,share_investor_ownership:=100*(sum_investors/homes_owned),]
dt_tableown <- dt_tableown[,-c(3,4)]
## average across all data for purchases by year
dt_table_ave_own <- dtown[,.(sum_investors=sum(investor_purchase),homes_owned=.N),by=list(int_year)]
dt_table_ave_own <- dt_table_ave_own[,share_investor_ownership:=100*(sum_investors/homes_owned),]
dt_table_ave_own <- dt_table_ave_own[,-c(2,3)]
### small investor ownership
dtownsmall <- data.table(int_year=timetable$YEAR, 
                    n_sales_year=timetable$PROPERTIES_OWNED_THIS_YEAR, 
                    school=timetable$schnam,
                    owner=timetable$OWNER)
dtownsmall <- dtownsmall[, investor_purchase:=0]
dtownsmall <- dtownsmall[owner %in% small_investor_names, investor_purchase:=1]
#investor purchases by schools
dt_tableownsmall <- dtownsmall[,.(sum_investors=sum(investor_purchase),homes_owned=.N),by=list(int_year,school)]
dt_tableownsmall <- dt_tableownsmall[,share_small_investor_ownership:=100*(sum_investors/homes_owned),]
dt_tableownsmall <- dt_tableownsmall[,-c(3,4)]
## average across all data for purchases by year
dt_table_ave_ownsmall <- dtownsmall[,.(sum_investors=sum(investor_purchase),homes_owned=.N),by=list(int_year)]
dt_table_ave_ownsmall <- dt_table_ave_ownsmall[,share_small_investor_ownership:=100*(sum_investors/homes_owned),]
dt_table_ave_ownsmall <- dt_table_ave_ownsmall[,-c(2,3)]
### medium investor ownership
dtownmedium <- data.table(int_year=timetable$YEAR, 
                         n_sales_year=timetable$PROPERTIES_OWNED_THIS_YEAR, 
                         school=timetable$schnam,
                         owner=timetable$OWNER)
dtownmedium <- dtownmedium[, investor_purchase:=0]
dtownmedium <- dtownmedium[owner %in% medium_investor_names, investor_purchase:=1]
#investor purchases by schools
dt_tableownmedium <- dtownmedium[,.(sum_investors=sum(investor_purchase),homes_owned=.N),by=list(int_year,school)]
dt_tableownmedium <- dt_tableownmedium[,share_medium_investor_ownership:=100*(sum_investors/homes_owned),]
dt_tableownmedium <- dt_tableownmedium[,-c(3,4)]
## average across all data for purchases by year
dt_table_ave_ownmedium <- dtownmedium[,.(sum_investors=sum(investor_purchase),homes_owned=.N),by=list(int_year)]
dt_table_ave_ownmedium <- dt_table_ave_ownmedium[,share_medium_investor_ownership:=100*(sum_investors/homes_owned),]
dt_table_ave_ownmedium <- dt_table_ave_ownmedium[,-c(2,3)]
### large investor ownership
dtownlarge <- data.table(int_year=timetable$YEAR, 
                          n_sales_year=timetable$PROPERTIES_OWNED_THIS_YEAR, 
                          school=timetable$schnam,
                          owner=timetable$OWNER)
dtownlarge <- dtownlarge[, investor_purchase:=0]
dtownlarge <- dtownlarge[owner %in% large_investor_names, investor_purchase:=1]
#investor purchases by schools
dt_tableownlarge <- dtownlarge[,.(sum_investors=sum(investor_purchase),homes_owned=.N),by=list(int_year,school)]
dt_tableownlarge <- dt_tableownlarge[,share_large_investor_ownership:=100*(sum_investors/homes_owned),]
dt_tableownlarge <- dt_tableownlarge[,-c(3,4)]
## average across all data for purchases by year
dt_table_ave_ownlarge <- dtownlarge[,.(sum_investors=sum(investor_purchase),homes_owned=.N),by=list(int_year)]
dt_table_ave_ownlarge <- dt_table_ave_ownlarge[,share_large_investor_ownership:=100*(sum_investors/homes_owned),]
dt_table_ave_ownlarge <- dt_table_ave_ownlarge[,-c(2,3)]

### PLOTTING PURCHASES BY INVESTORS
dt_investors_purchased <- data.table(Year=dt_table_ave$int_year, All=dt_table_ave$share_investor_purchases,
                                     Small=dt_table_avesmall$share_small_investor_purchases, 
                                     Medium=dt_table_avemed$share_medium_investor_purchases,
                                     Large=dt_table_avelarge$share_large_investor_purchases)


ggplot(data=dt_investors_purchased, aes(x=Year)) + 
  geom_point(y = dt_investors_purchased$All,size=4, aes(shape="All Investors")) +
  geom_line(aes(y = All), size=1, alpha=.6) + 
  geom_point(y = dt_investors_purchased$Small,size=4, aes(shape="Small Investors (3-9 Properties)")) +
  geom_line(aes(y = Small), size=1, alpha=.6) +
  geom_point(y = dt_investors_purchased$Medium,size=4, aes(shape="Medium Investors (10-99 Properties)")) +
  geom_line(aes(y = Medium), size=1, alpha=.6) +
  geom_point(y = dt_investors_purchased$Large,size=4, aes(shape="Large Investors (100-1000+)")) +
  geom_line(aes(y = Large), size=1, alpha=.6) + theme_grey()+
  xlab("Year") + ylab("Share of Properties Purchased")+theme(legend.position="right") +     
  geom_text(aes(x = 2007, y = 38, label = "All Investors"),size=4) + 
  geom_text(aes(x = 2010.2, y = 25, label = "Small Investors (3-9 Properties)"),size=4) +
  geom_text(aes(x = 2013.5, y = 6, label = "Large Investors (100-1000+)"),size=4) +
  geom_text(aes(x = 2009.25, y =12, label = "Medium Investors (10-99 Properties)"),size=4) +
  labs(title="Share of Properties Purchased by Investors", caption = "Data from Maricopa County Sales Affidavits")+ 
  scale_y_continuous(breaks = round(seq(0, 60, by = 5),1),labels = function(x) paste0(x,"%"))
### OWNERSHIP
dt_investors_owned <- data.table(Year=dt_table_ave_own$int_year, AlL=dt_table_ave_own$share_investor_ownership,
                                       Small=dt_table_ave_ownsmall$share_small_investor_ownership, 
                                       Medium=dt_table_ave_ownmedium$share_medium_investor_ownership,
                                       Large=dt_table_ave_ownlarge$share_large_investor_ownership)

write.csv(round(dt_investors_owned, digits=2), file="shareowned.csv")
write.csv(round(dt_investors_purchased, digits=2), file="sharepurchased.csv")
write.csv(final, file="final_pan_rounded.csv")

ggplot(data=dt_investors_owned, aes(x=Year)) + 
  geom_point(y = dt_investors_owned$AlL,size=4, aes(shape="All Investors")) +
  geom_line(aes(y = dt_investors_owned$AlL), size=1, alpha=.6) + 
  geom_point(y = dt_investors_owned$Small,size=4, aes(shape="Small Investors (3-9 Properties)")) +
  geom_line(aes(y = dt_investors_owned$Small), size=1, alpha=.6) +
  geom_point(y = dt_investors_owned$Medium,size=4,aes(shape="Medium Investors (10-99 Properties)")) +
  geom_line(aes(y = dt_investors_owned$Medium), size=1, alpha=.6) +
  geom_point(y = dt_investors_owned$Large,size=4, aes(shape="Large Investors (100-1000+)")) +
  geom_line(aes(y = dt_investors_owned$Large), size=1, alpha=.6) + theme_grey()+
  xlab("Year") + ylab("Share of Homes Owned by Investors")+theme(legend.position="right") +     
  geom_text(aes(x = 2007, y = 27, label = "All Investors"),size=4) + 
  geom_text(aes(x = 2011.25, y = 20, label = "Small Investors (3-9 Properties)"),size=4) +
  geom_text(aes(x = 2013.5, y = 2.5, label = "Large Investors (100-1000+)"),size=4) + 
  geom_text(aes(x = 2009.25, y = 7, label = "Medium Investors (10-99 Properties)"),size=4) +
  labs(title="Share of Homes Owned by Investors", caption = "Data from Maricopa County Sales Affidavits") +
  scale_y_continuous(breaks = round(seq(0, 60, by = 5),1),labels = function(x) paste0(x, "%"))




