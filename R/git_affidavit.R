library(data.table)
#install.packages("tidyverse")
#install.packages("tidyr")
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
#library(visdat)
#library(caret)
#library(recipes)
#library(h2o) 
homesales <- readRDS("phoenix_data_grantee_frequency.rds")

### SKIP THIS; RENTAL DATA
rentalprops <- readRDS("rental_data_w_titles.rds")
rentalprops <- separate(rentalprops, col = "DATE\xcaFIRST REGISTERED", into = c("MONTH","DAY", "YEAR"), sep = "/",convert=TRUE)
#rentalprops[,"DATE\xcaFIRST REGISTERED"]
### separate dates for rental props
#length(which(homesales[,"GRANTEE OWNER NAME"]== "BEAZER HOMES HOLDINGS CORPORATION"))
homesales <- separate(homesales, col= "SALE DATE (MMYYYY)",into = c('SALE MONTH', 'SALE YEAR'), sep = -4, convert=TRUE)
homesales <- separate(homesales, col= "DEED DATE (MMDDYYYY)",into = c('DEED MONTH', 'DEED YEAR'), sep = -4, convert=TRUE)
homesales <- separate(homesales, col= "DEED MONTH",into = c('DEED MONTH', 'DEED DAY'), sep = -2, convert=TRUE)
#head(homesales)

##### SKIP THIS; PLOTTING FREQUENCY OF PURCHASES

homesales <- homesales[, n_grantee:=.N, by = 'GRANTEE OWNER NAME'] ### adds a count of number of properties purchased over the dataset
#### REMOVE SALES TO SELF ####################
homesalesSELFLESS <- homesales[`GRANTOR OWNER NAME` != `GRANTEE OWNER NAME`]
# inserts column with count of buyers data.table code
homesalesSELFLESS <- homesalesSELFLESS[, n_grantee:=.N, by = 'GRANTEE OWNER NAME']
head(homesalesSELFLESS)
dt1 <- homesalesSELFLESS[n_grantee!=1] ## bought more than one home
dt10 <- homesalesSELFLESS[n_grantee>10]
##plotting histograms of buyers
ggplot(homesalesSELFLESS, aes(x=n_grantee)) + 
  geom_histogram(color = "dodger blue", fill = "dodger blue", alpha=0.75, binwidth=300) +
  theme_minimal() + theme(legend.position="top") +
  scale_x_continuous(name="Properties Purchased", breaks = seq(0, max(homesalesSELFLESS[,n_grantee]), by = 1000)) +
  #scale_y_continuous(name="Count", breaks = seq(0, max(length(homesalesSELFLESS[,n_grantee])), by = 100000))
  scale_y_log10()

ggplot(dt1, aes(x=n_grantee)) + 
  geom_histogram(color = "dodger blue", fill = "dodger blue", alpha=0.75, binwidth=300) +
  theme_minimal() + theme(legend.position="top") +
  scale_x_continuous(name="Properties Purchased", breaks = seq(0, max(dt1[,n_grantee]), by = 1000)) +
  scale_y_continuous(name="Count", breaks = seq(0, max(length(dt1[,n_grantee])), by = 50000))

ggplot(dt10, aes(x=n_grantee)) + 
  geom_histogram(color = "dodger blue", fill = "dodger blue", alpha=0.75, binwidth=300) +
  theme_minimal() + theme(legend.position="top") +
  scale_x_continuous(name="Properties Purchased", breaks = seq(0, max(dt10[,n_grantee]), by = 1000)) +
  scale_y_continuous(name="Count", breaks = seq(0, max(length(dt10[,n_grantee])), by = 10000))
#geom_density(alpha=.2, fill="#FF6666") append aes(y=..density..) to geom_histogram()

########################
#colnames(homesales)
parcel_status <- data.table(parcel_id=homesales[,`PARCEL NUMBER           `],
                            YEAR=homesales[,`SALE YEAR`], 
                            SELLER=homesales[,`GRANTOR OWNER NAME`],
                            BUYER=homesales[,`GRANTEE OWNER NAME`],
                            OWNER=homesales[,`GRANTEE OWNER NAME`],
                            PROP_TYPE=homesales[,`PROPERTY TYPE DESCRIPTION`])

##########
DT <- data.table(parcel_id=rep(unique(parcel_status[,parcel_id],),30)) ### building frame of data.table for 30 years of data using repeated 30 years of unique parcels
keycol <- "parcel_id"
setorderv(DT, keycol)

DT <- DT[, YEAR:=rep(seq(1986,2015),length(unique(parcel_status[,parcel_id])))]  ### filling 'YEARS' with 30 year sequence

DT_test <- DT

## originally was reduced to 1,000,000 obs for coding tests; now using full dataset
#101-01-011	2006	SUNDT CONSTRUCTION INC	AVONDALE CORPORATE CENTER I LLC
# set the ON clause as keys of the tables:
setkey(parcel_status,parcel_id, YEAR)
setkey(DT_test,parcel_id, YEAR)
# perform the join using the merge function
DT_test <- merge(DT_test,parcel_status, all.x=TRUE)
DT_test <- DT_test %>% group_by(parcel_id) %>% fill(BUYER, .direction = "down")  ###fills each parcel id for buyers
DT_test[,'OWNER'] <- DT_test[,'BUYER'] #### making buyer the owner for valid boxes
DT_test <- DT_test %>% group_by(parcel_id) %>% fill(SELLER, .direction = "up" )  ###fills each parcel id for seller
DT_test$OWNER[which(is.na(DT_test$BUYER))] <- DT_test$SELLER[which(is.na(DT_test$BUYER))] ### fills current owner for empty buyers
##DT_test$OWNER[which(is.na(DT_test$SELLER))] <- DT_test$BUYER[which(is.na(DT_test$SELLER))] ### fills current owner (redundant/unneccessary)
head(DT_test)
####
DT_test <- DT_test[,-c(3,4)]  ### dropping redundant buyer seller columns
DT_test <- DT_test %>% group_by(parcel_id) %>% fill(PROP_TYPE, .direction = "updown")###filling in property types
#DT_test <- DT_test %>% group_by(OWNER) %>% fill(TOTAL_PROPS_PURCHASED, .direction = "updown")  ## adds the count of total homes; must add freq.grantee
DT_test <- as.data.table(DT_test) ### reinitializing as data.table
###### getting first year of parcel to remove non-existent dates
min_years <- data.table(parcel_status[,list(First_Sale = min(YEAR)), by = parcel_id])
### merging new column
is.data.table(DT_test)
setkey(DT_test, parcel_id)
setkey(min_years,parcel_id) ## ignore warnings
# perform the join using the merge function
DT_test <- merge(DT_test, min_years, all.x=TRUE)
DT_test<-DT_test[!(DT_test$YEAR < (DT_test$First_Sale - 1)),]  ### pruning non-existent years (first sale in 2006; removes 1986-2004)

#
#
#

saveRDS(DT_test, file="Phoenix_Owners_Timetable.rds")

saveRDS(parcel_status, file="Phoenix_Parcel_Status.rds")

########### ADDING LAT AND LONG
timetable <- readRDS("Phoenix_Owners_Timetable.rds")
timetable$OWNER[(timetable$OWNER == "BEAZER HOMES SALES  INC.")] = "BEAZER HOMES SALES INC" ### fixing spelling error
timetable <- timetable[(timetable$First_Sale >= 2007),] #only for 2007+
### add merge lat and longs  SKIP THIS FOR NOW
setkey(timetable, parcel_id)
setkey(fulldata, parcel_id)
timetable <- merge(timetable, fulldata, all.x=TRUE)
timetable <- timetable[which(is.na(timetable[,6])==FALSE),]
timetable <- timetable[,c(-7)]
#saveRDS(timetable, file="Phoenix_Owners_Timetable_w_LatLong.rds")
##### counting owners by year

for (i in 2006:2015) {
  timetable[(timetable$YEAR==i), PROPERTIES_OWNED_THIS_YEAR:=.N, by=OWNER]
}

### RANKING
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR > 1000), SIZE:='Z', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR < 1000) , SIZE:='Y', by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR <100), SIZE:=X, by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR <50), SIZE:=W, by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR <10), SIZE:=E, by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR == 4), SIZE:=D, by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR == 3), SIZE:=C, by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR == 2), SIZE:=B, by=PROPERTIES_OWNED_THIS_YEAR]
timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR == 1), SIZE:=A, by=PROPERTIES_OWNED_THIS_YEAR]

for (i in 2006:2015) {
  timetable[(timetable$YEAR==i), NUM_OF_INVESTORS_BY_SIZE_THIS_YEAR:=.N, by=SIZE] #### number of non,small, medium, large investors THIS YEAR
}
#### subsetting 

timetable <- timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR > 2),] #### owns more than two properties in one year
timetable <- timetable[(timetable$PROPERTIES_OWNED_THIS_YEAR <2000),] #### owns more than two properties in one year

ggplot(timetable, aes(x=PROPERTIES_OWNED_THIS_YEAR)) + 
  geom_histogram(color= "dodgerblue", alpha=0.98, bins=100) + theme_minimal()+
  theme(legend.position="top") +facet_grid(~YEAR) +
  scale_y_continuous("Number of Individual Owners With That Count", breaks = seq(0, max(length(timetable[,timetable$PROPERTIES_OWNED_THIS_YEAR])), by = 1000))


## BY RANK
ggplot(timetable, aes(x=SIZE)) + 
  geom_histogram(color= "dodgerblue", alpha=0.98) + theme_minimal()+
  theme(legend.position="top") + facet_grid(~YEAR) + scale_y_continuous("Number of Individual Owners With That Count", breaks = seq(0, max(timetable$NUM_OF_INVESTORS_BY_SIZE_THIS_YEAR), by = 10000))


####  PLOTTING EACH PROPERTY BY THE OWNERS SHARE OF HOMES OWNED
timetable[, SHARE_PROPS:=PROPERTIES_OWNED_THIS_YEAR/NUM_OF_PROPS_THIS_YEAR, by=YEAR] ## adding the proportion of propeties owned this year
for (i in 2006:2015) {
  timetable[(timetable$YEAR==i), NUM_OF_PROPS_THIS_YEAR:=.N, by=YEAR] #### number of properties owned this year
}
test <- timetable[1:790000,]
ggplot(test,aes(x=YEAR,y=SHARE_PROPS,group=parcel_id)) + geom_line()


counts = timetable[,.N,by=.(SIZE,YEAR)]

counts[order(YEAR, SIZE)]

ggplot(counts,aes(x=YEAR,y=N,color=SIZE,group=SIZE)) + geom_line()
