library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)

data <- read.csv("LAND_SALES_ALL.CSV")
data <- data[,c(2,62,63)]
data7 <- read.csv("IMPSALES_2007.CSV")
data7 <- data7[,c(2,24,25)]
data8 <- read.csv("IMPSALES_2008.CSV")
data8 <- data8[,c(2,24,25)]
data9 <- read.csv("IMPSALES_2009.CSV")
data9 <- data9[,c(2,24,25)]
data10 <- read.csv("IMPSALES_2010.CSV")
data10 <- data10[,c(2,24,25)]
data11 <- read.csv("IMPSALES_2011.CSV")
data11 <- data11[,c(2,24,25)]
data12 <- read.csv("IMPSALES_2012.CSV")
data12 <- data12[,c(2,24,25)]
data13 <- read.csv("IMPSALES_2013.CSV")
data13 <- data13[,c(2,24,25)]
data14 <- read.csv("IMPSALES_2014.CSV")
data14 <- data14[,c(2,24,25)]
data15 <- read.csv("IMPSALES_2015.CSV")
data15 <- data15[,c(2,24,25)]
data16 <- read.csv("IMPSALES_2016.CSV")
data16 <- data16[,c(2,24,25)]
data17 <- read.csv("IMPSALES_2017.CSV")
data17 <- data17[,c(2,24,25)]
data18 <- read.csv("IMPSALES_2018.CSV")
data18 <- data18[,c(2,24,25)]
colnames(data) <- c("PARCELID","LONGITUDE", "LATITUDE")
fulllatlong<- rbind(data,data9,data11)
fulllatlong<- rbind(data,data7,data8,data9,data10,data11,data12,data13,data14,data15,data16,data17,data18)
fulllatlong[,1] <- gsub("(\\w{3})(\\w{2})(\\w{4})$","\\1-\\2-\\3",fulllatlong[,1])
fulllatlong[,1] <- gsub("(\\d{3})(\\d{2})(\\d{3})$","\\1-\\2-\\3",fulllatlong[,1])
#head(fulllatlong)
fulllatlong <-as.data.table(fulllatlong)
colnames(fulllatlong) <- c("parcel_id","long","lat")
fulllatlong <- distinct(fulllatlong, parcel_id, .keep_all = TRUE)
fulllatlong <- fulllatlong[is.na(fulllatlong$long) == FALSE,] ### removing parcels with no lat long
saveRDS(fulllatlong, file="Parcel_IDs_w_Lat_Long.rds")
setwd("~/Desktop")
fulllatlong <- readRDS("Parcel_IDs_w_Lat_Long.rds")





