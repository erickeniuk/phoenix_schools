library(rgdal)
#install.packages('rgdal')
#install.packages('mapview')
library(mapview)
library(sf)
library(dplyr)
library(rgdal)
library(mapview)


nhood <- st_read('SABS_1516_High.shp')
nhood <- nhood[which(nhood$stAbbrev == 'AZ'),] ## only arizona schols
#nhood

con_hood = st_transform(nhood, CRS("+proj=longlat +datum=WGS84")) ## converting long and lat to merc


head(con_hood)
districts <- timetable[,c(1,6,7)]
districts <-  st_as_sf(districts, coords = c("long", "lat"), crs = 4326)
head(districts)

final_parcels_schools <- st_join(districts, con_hood['schnam'], join = st_intersects)

final_parcels_schools <- as.data.frame(final_parcels_schools)

saveRDS(final_parcels_schools, file="parcels_schools.rds")



