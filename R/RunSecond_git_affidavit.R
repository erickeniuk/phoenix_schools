library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
###
homesales <- readRDS("phoenix_data_grantee_frequency.rds")
homesales <- separate(homesales, col= "SALE DATE (MMYYYY)",into = c('SALE MONTH', 'SALE YEAR'), sep = -4, convert=TRUE)
### add merge lat and longs to original dataset
homesales$`GRANTEE OWNER NAME`[(homesales$`GRANTEE OWNER NAME` == "BEAZER HOMES SALES  INC.")] = "BEAZER HOMES SALES INC" ### fixing spelling error
homesales <- homesales[(homesales$`SALE YEAR` >= 2000),] #only for 2000+
head(homesales)
homesales <- homesales[,c(1,3,4,10,11,15:46)]
names(homesales)[1] <- "parcel_id" ## rename to parcelid
### add merge lat and longs  SKIP THIS FOR NOW
fulllatlong <- readRDS("Parcel_IDs_w_Lat_Long.rds")
homesales <- homesales[fulllatlong, on = c("parcel_id"="parcel_id"),]  ## adding shares of investors
#####
homesales <- homesales[,c(1,2,3,7, 14, 23, 31,36, 37, 38, 39)]
homesales <- as.data.table(homesales)
homesales <- homesales[!is.na(homesales$long),]
##########
parcel_status <- data.table(parcel_id=homesales$parcel_id,
                            YEAR=homesales$`SALE YEAR`, 
                            SELLER=homesales$`GRANTOR OWNER NAME`,
                            BUYER=homesales$`GRANTEE OWNER NAME`,
                            OWNER=homesales$`GRANTEE OWNER NAME`)

########## creating timetable of who owned what parcel in what year
DT <- data.table(parcel_id=rep(unique(parcel_status[,parcel_id],),16)) ### building frame of data.table for 30 years of data using repeated 30 years of unique parcels
setorderv(DT, "parcel_id") # sorts by parcel id
DT <- DT[, YEAR:= rep(2000:2015), by = c("parcel_id")]  ### filling 'YEARS' with 16 year sequence
setkey(parcel_status, parcel_id, YEAR)
setkey(DT,parcel_id, YEAR)
# perform the join using the merge function
DT <- merge(DT,parcel_status, all.x=TRUE)
DT <- DT %>% group_by(parcel_id) %>% fill(BUYER, .direction = "down") ###fills each parcel id for buyers
DT$OWNER <- DT$BUYER #### making buyer the owner for valid boxes
DT <- DT %>% group_by(parcel_id) %>% fill(SELLER, .direction = "up" )  ###fills each parcel id for seller
DT$OWNER[which(is.na(DT$BUYER))] <- DT$SELLER[which(is.na(DT$BUYER))] ### fills current owner for empty buyers
DT <- DT[,-c(3,4)]  ### dropping redundant buyer seller columns
DT <- as.data.table(DT) ### reinitializing as data.table
###### getting first year of parcel to remove non-existent dates
min_years <- data.table(parcel_status[,list(First_Sale = min(YEAR)), by = parcel_id])
### merging new column
setkey(DT, parcel_id)
setkey(min_years,parcel_id) ## ignore warnings
# perform the join using the merge function
DT <- merge(DT, min_years, all.x=TRUE)
DT<-DT[!(DT$YEAR < (DT$First_Sale - 1)),]  ### pruning non-existent years (first sale in 2006; removes 1986-2004; assumed built/owned in 2005)

#saveRDS(DT, file="Phoenix_Owners_Timetable.rds")
#saveRDS(parcel_status, file="Phoenix_Parcel_Status.rds")
########### ADDING LAT AND LONG to timetable
timetable <- readRDS("Phoenix_Owners_Timetable.rds")
timetable$OWNER[(timetable$OWNER == "BEAZER HOMES SALES  INC.")] = "BEAZER HOMES SALES INC" ### fixing spelling error
timetable <- timetable[(timetable$First_Sale >2005),] #only for 2006+
### add merge lat and longs  SKIP THIS FOR NOW
setkey(timetable, parcel_id)
setkey(fulllatlong, parcel_id)
timetable <- merge(timetable, fulllatlong, all.x=TRUE)
timetable <- timetable[which(is.na(timetable[,6])==FALSE),]
timetable <- timetable[,-c(timetable$First_Sale)]
#saveRDS(timetable, file="Phoenix_Owners_Timetable_w_LatLong.rds")



