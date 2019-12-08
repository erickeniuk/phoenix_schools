
library(rgdal)
library(mapview)
library(plyr)
library(sp)
library(raster)
library(maps)
library(sf)
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
library(ggmap)

ggmap::register_google(key = "AIzaSyDNqrQa3Ap77vEicmeoeL8pfmTRT35lPnk")
setwd("~/Dropbox/attendanceZoneBoundaries/SABS_1516_SchoolLevels")
schoolbounds <- st_read('SABS_1516_Primary.shp') ## loading boundaries of primary schools
schoolbounds<- schoolbounds[which(schoolbounds$stAbbrev == 'AZ'),] ## only arizona primary schools
setwd("~/Desktop")
final <- read.csv("phoenix_panel_final.csv")
final$med_price <- final$med_price/1000
names(final)[5] <- "schnam"
### getting change of investors
schoolchange <- final[final$year==2010 | final$year==2015,]
schoolchange <- as.data.table(schoolchange)
schoolchange$share_investor_ownership[schoolchange$year==2010] <- -1*(schoolchange$share_investor_ownership[schoolchange$year==2010])
schoolchange$share_small_investor_ownership[schoolchange$year==2010] <- -1*(schoolchange$share_small_investor_ownership[schoolchange$year==2010])
schoolchange$share_investor_purchases[schoolchange$year==2010] <- -1*(schoolchange$share_investor_purchases[schoolchange$year==2010])
schoolchange$share_small_investor_purchases[schoolchange$year==2010] <- -1*(schoolchange$share_small_investor_purchases[schoolchange$year==2010])
## change in percentile rank
schoolchange$rankmath[schoolchange$year==2010] <- -1*(schoolchange$rankmath[schoolchange$year==2010])
schoolchange$rankread[schoolchange$year==2010] <- -1*(schoolchange$rankread[schoolchange$year==2010])
schoolchange[,countschool:=.N, by=sch_id]
schoolchange <- schoolchange[schoolchange$countschool!=1,]

changes <- schoolchange[,.(ChangeOwn=sum(share_investor_ownership),
                           ChangePurch=sum(share_investor_purchases),
                           ChangeSmallOwn=sum(share_small_investor_ownership),
                           ChangeSmallPurch=sum(share_small_investor_purchases),
                           ChangeMath=sum(rankmath),
                           ChangeRead=sum(rankread)),by=(sch_id)]

changes <- as.data.frame(changes)
final <- merge(final,changes,by='sch_id')
###merge onto shp file
schoolbounds <- merge(schoolbounds, final, by='schnam', all.x=TRUE)
schoolbounds <- schoolbounds[!is.na(schoolbounds$share_investor_ownership),]
schoolbounds <- st_transform(schoolbounds, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))


schoolbounds <- schoolbounds[schoolbounds$year==2010 | schoolbounds$year==2015,]
schoolbounds10 <- schoolbounds[schoolbounds$year==2010,]
schoolbounds15 <- schoolbounds[schoolbounds$year==2015,]

# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
schools <- st_transform(schoolbounds15, 3857)

map <- get_googlemap(center = c(lon = -112, lat = 33.4),
                     zoom=10, scale=2,
                     maptype='hybrid',
                     color = 'color')
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

mid <- mean(schools$share_investor_ownership)

map <- ggmap_bbox(map) 

summary(schools$ChangeOwn)
summary(schools$ChangePurch)
summary(schools$ChangeSmallOwn)
summary(schools$ChangeSmallPurch)
summary(schools$ChangeMath)
summary(schools$ChangeRead)


schools$ChangeOwn <- schools$ChangeOwn/100
schools$ChangeSmallOwn <- schools$ChangeSmallOwn/100
schools$ChangePurch <- schools$ChangePurch/100
schools$ChangeSmallPurch <- schools$ChangeSmallPurch/100
schools$ChangeMath <- schools$ChangeMath/100
schools$ChangeRead <- schools$ChangeRead/100

ggmap(map) + ggtitle("Change in Math Ranking")+
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = schools, aes(fill = schools$ChangeMath), size=0.1,alpha=0.5, inherit.aes = FALSE)+
  scale_fill_gradient(low="brown2", high="green2",labels=scales::percent,limits = c(-.50,.60), breaks = c(-.50, -.25,0, .25,.60), 
                      guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE, 
                                              barwidth = 1.1,
                                              barheight = 11,title="Change in Math\nPercentile Ranking"))







###




###
ggmap(map) + ggtitle("Investor Purchases in 2015")+
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = schools, aes(fill = schools$share_investor_purchases), size=0.1,alpha=0.5, inherit.aes = FALSE)+
  scale_fill_gradient(low="dodgerblue2", high="brown2", limits = c(0,100), breaks = c(0, 25, 50, 75, 100),
                      guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE, 
                                              barwidth = 1.1,
                                              barheight = 11,title="Share of \nInvestor Purchases"))


ggmap(map) + ggtitle("Small Investor Purchases in 2015")+
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = schools, aes(fill = schools$share_small_investor_purchases), size=0.1,alpha=0.5, inherit.aes = FALSE)+
  scale_fill_gradient(low="dodgerblue2", high="brown2", limits = c(0,100), breaks = c(0, 25, 50, 75, 100),
                      guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE, 
                                              barwidth = 1.1,
                                              barheight = 11,title="Share of Small\nInvestor Purchases"))

ggmap(map) + ggtitle("Medium Investor Purchases in 2015")+
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = schools, aes(fill = schools$share_medium_investor_purchases), size=0.1,alpha=0.5, inherit.aes = FALSE)+
  scale_fill_gradient(low="dodgerblue2", high="brown2", limits = c(0,100), breaks = c(0, 25, 50, 75, 100),
                      guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE, 
                                              barwidth = 1.1,
                                              barheight = 11,title="Share of Medium\nInvestor Purchases"))

ggmap(map) + ggtitle("Large Investor Purchases in 2015")+
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = schools, aes(fill = schools$share_large_investor_purchases), size=0.1,alpha=0.5, inherit.aes = FALSE)+
  scale_fill_gradient(low="dodgerblue2", high="brown2", limits = c(0,100), breaks = c(0, 25, 50, 75, 100),
                      guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE, 
                                              barwidth = 1.1,
                                              barheight = 11,title="Share of Large\nInvestor Purchases"))

#+
length(which(schools$med_price>7))

ggmap(map) + ggtitle("Median Home Sale Price in 2015")+
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = schools, aes(fill = schools$med_price), size=0.1,alpha=0.9, inherit.aes = FALSE)+
  scale_fill_gradient(low="red2", high="green2", limits = c(0,500), breaks = c(0,150,350,500),
                      guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE, 
                                              barwidth = 1.1,
                                              barheight = 11,title="Median Home Sale Price\nin Thousands of Dollars"))


#### More graphs
final <- as.data.table(final)

scoreschange <- final[,.(mean_math=mean(math_pass, na.rm=TRUE), medianmath= median(math_pass, na.rm=TRUE),
                         mean_read=mean(reading_pass,na.rm=TRUE), medianread=median(reading_pass,na.rm=TRUE)), by= year]


ggplot(data=scoreschange, aes(x=year)) + 
  geom_point(y = scoreschange$mean_math,size=4, aes(shape="Mean Math Scores")) +
  geom_line(aes(y = scoreschange$mean_math), size=1, alpha=.6) +
  geom_point(y = scoreschange$mean_read,size=4, aes(shape="Mean Reading Scores")) +
  geom_line(aes(y = scoreschange$mean_read), size=1, alpha=.6) +
  xlab("Year") + ylab("Changes in Math and Reading Scores")+theme(legend.position="right") +     
  geom_text(aes(x = 2012.25, y = 63, label = "Mean Math Scores"),size=4) +
  geom_text(aes(x = 2012.5, y = 75, label = "Mean Reading Scores"),size=4) +
  labs(title="Percent Passing", caption = "Data from Maricopa County Sales Affidavits") +
  scale_y_continuous(breaks = round(seq(25, 100, by = 5),1),labels = function(x) paste0(x, "%")) +
  geom_vline(xintercept = c(2014,2015), linetype="dotted", 
             color = "black", size=1.5)


gini <- read.csv("SIPOVGINIUSA.csv")
gini <- gini[gini$SIPOVGINIUSA!='.',]
names(gini)[c(1,2)] = c("Year", "Gini Index")
gini <- separate(gini, col= "Year",into = 'year', sep = 4, convert=TRUE)

gini$`Gini Index` <- as.numeric(levels(gini$`Gini Index`))[gini$`Gini Index`]

ggplot(data=gini, aes(x=year)) + 
  geom_point(y= gini$`Gini Index`, size=4) +
  geom_line(aes(y= gini$`Gini Index`), size=1, alpha=0.6) + ylab("")+
  ggtitle("Change in Gini Index: 1979-2016") +xlab("") +
  theme(text = element_text(size=24)) +
  scale_y_continuous(breaks = round(seq(34, 42, by = 2),1))

incomegrowth <- read.csv("MEHOINUSA672N.csv")
incomegrowth <- separate(incomegrowth, col= "DATE",into = 'Year', sep = 4, convert=TRUE)
incomegrowth <- incomegrowth[incomegrowth$Year>1987,]

gdpgrowth <- read.csv("GDPC1.csv")
gdpgrowth <- separate(gdpgrowth, col= "DATE",into = 'Year', sep = 4, convert=TRUE)

ggplot(data=gdpgrowth, aes(x=Year)) + 
  geom_point(y= incomegrowth$MEHOINUSA672N_PC1, size=2,aes(shape="Real Median Income Growth"), col="forestgreen") +
  geom_line(aes(y= incomegrowth$MEHOINUSA672N_PC1), col="forestgreen", size=1, alpha=0.9) + ylab("")+
  geom_point(y= gdpgrowth$GDPC1_PC1, size=2, aes(shape="Real GDP Growth"), color="dodgerblue3") +
  geom_line(aes(y= gdpgrowth$GDPC1_PC1), size=1, color="dodgerblue3", alpha=0.9) + ylab("")+
  ggtitle("Real GDP Growth vs. Real Median Income Growth") +xlab("") + 
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.25) +
  geom_text(aes(x = 1998, y = -3.1, label = "Real Median Income Growth"),size=5, color="forestgreen") +
  geom_text(aes(x = 2004.3, y = 4.15, label = "Real GDP Growth"),size=5, color="dodgerblue3") +
  scale_y_continuous(breaks = round(seq(-4, 4, by = 1),1),labels = function(x) paste0(x, "%"))+
  scale_x_continuous(breaks = round(seq(1988, 2018, by = 10),1))+
  theme(text = element_text(size=16)) + theme(legend.position = "none")

homes <- read.csv("CSUSHPINSA.csv")
homes <- separate(homes, col= "DATE",into = 'Year', sep = 4, convert=TRUE)
homes <- homes[homes$Year!=2019,]
homes$CSUSHPINSA_PC1 <- as.numeric(levels(homes$CSUSHPINSA_PC1))[homes$CSUSHPINSA_PC1]


ggplot(data=incomegrowth, aes(x=Year)) + 
  geom_point(y= incomegrowth$MEHOINUSA672N_PC1, size=2,aes(shape="Real Median Income Growth"),col="forestgreen") +
  geom_line(aes(y= incomegrowth$MEHOINUSA672N_PC1), col="forestgreen", size=1, alpha=0.9) + ylab("")+
  geom_point(y= homes$CSUSHPINSA_PC1, size=2, aes(shape="Housing Price Index Growth"), col="brown2") +
  geom_line(aes(y= homes$CSUSHPINSA_PC1), size=1, color="brown2", alpha=0.9) + ylab("")+
  ggtitle("Real Median Income Growth vs. House Price Index Growth") +xlab("") + 
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.25) +
  geom_text(aes(x = 1998, y = -3.1, label = "Real Median Income Growth"),size=5, color="forestgreen") +
  geom_text(aes(x = 2004, y = 6.6, label = "Housing Price Index Growth"),size=5, color="brown2") +
  scale_y_continuous(breaks = round(seq(-10, 15, by = 5),1),labels = function(x) paste0(x, "%"))+
  scale_x_continuous(breaks = round(seq(1988, 2018, by = 10),1))+
  theme(text = element_text(size=16)) + theme(legend.position = "none")


mean(gdpgrowth$GDPC1_PC1)
mean(incomegrowth$MEHOINUSA672N_PC1)
mean(gdpgrowth$GDPC1_PC1[gdpgrowth$Year>2008])
mean(incomegrowth$MEHOINUSA672N_PC1[incomegrowth$Year>2008])

mean(homes$CSUSHPINSA_PC1)

mean(incomegrowth$MEHOINUSA672N_PC1[incomegrowth$Year>2008 & incomegrowth$Year<2014])
mean(homes$CSUSHPINSA_PC1[homes$Year>2009 & homes$Year <2014])

sum(incomegrowth$MEHOINUSA672N_PC1[incomegrowth$Year>2008 & incomegrowth$Year<2014])
sum(homes$CSUSHPINSA_PC1[homes$Year>2006 & homes$Year <2012])
sum(homes$CSUSHPINSA_PC1[homes$Year>2011 & homes$Year <2015])
?geom_hline


newhomes <- read.csv("COMPUTSA.csv")
newhomes <- separate(newhomes, col= "DATE",into = 'Year', sep = 4, convert=TRUE)

population <- read.csv("POPTHM.csv")
population <- separate(population, col= "DATE",into = 'Year', sep = 4, convert=TRUE)


ggplot(data=population, aes(x=Year)) + 
  geom_point(y= population$POPTHM_CH1, size=2,aes(shape="Population Growth"),col="dodgerblue4") +
  geom_line(aes(y= population$POPTHM_CH1), col="dodgerblue4", size=1, alpha=0.9) + ylab("")+
  geom_point(y= population$POPTHM_CH1/2, size=2,aes(shape="Population Growth Halved"),col="dodgerblue1") +
  geom_line(aes(y= population$POPTHM_CH1/2),lty=2, col="dodgerblue1", size=1, alpha=0.7) + ylab("")+
  geom_point(y= newhomes$COMPUTSA, size=2, aes(shape="New Housing Units Built"), col="grey1") +
  geom_line(aes(y= newhomes$COMPUTSA), size=1, color="grey1", alpha=0.9) + ylab("In Thousands")+
  ggtitle("New Housing Units Built vs. Population Growth") +xlab("") + 
  geom_text(aes(x = 2009, y = 3000, label = "Population Growth"),size=6, color="dodgerblue4") +
  geom_text(aes(x = 1998, y = 1000, label = "New Housing Units Built"),size=6, color="grey1") +
  geom_text(aes(x = 1996, y = 1850, label = "Population Growth Halved"),size=6, color="dodgerblue1") +
  scale_y_continuous(breaks = round(seq(0, 3500, by = 500),1))+
  scale_x_continuous(breaks = round(seq(1988, 2018, by = 10),1)) +
  theme(text = element_text(size=16)) + theme(legend.position = "none")

mean(newhomes$COMPUTSA)
mean(population$POPTHM_CH1)
sum(newhomes$COMPUTSA)
sum(population$POPTHM_CH1)
sum(population$POPTHM_CH1)/2
