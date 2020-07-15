
# Load map packages
library("maps")
library("mapproj")
library("mapplots")
library("mapdata")
library("ggmap")
#don't need all these packages just for this one map, but I use them later for other map stuff in the script that I am not including here

#Usage:
#get_map(location = c(lon = -95.3632715, lat = 29.7632836),
#zoom = "auto", scale = "auto", 
#maptype = c("terrain", "terrain-background", "satellite", "roadmap", 
#"hybrid", "toner", "watercolor", "terrain-labels", "terrain-lines", "toner-2010", 
#"toner-2011", "toner-background", "toner-hybrid", "toner-labels", "toner-lines", "toner-lite"), 
#source = c("google", "osm", "stamen", "cloudmade"), 
#force = ifelse(source == "google", TRUE, TRUE), messaging = FALSE, 
#urlonly = FALSE, filename = "ggmapTemp", crop = TRUE, color = c("color", "bw"), language = "en-EN", api_key)

EPGT_latlong<-read.csv("Lat_long_EPGT.csv")
EPGT_latlong$mean_d15N_scale<-EPGT_latlong$mean_d15N*0.4
EPGT_latlong$mean_d15N_scale_1<-EPGT_latlong$mean_d15N*0.75
#Entire Study Region Map
description<-"EPacRegion"
myLocation <- c(-175, -45, -62, 45)
#bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat

myMap <- get_map(location=myLocation, source="google",
                  maptype="hybrid", crop=TRUE) 

pdf(paste(description, "_Gmap", ".pdf",sep=""), width = 11, height = 8.5)
ggmap(myMap) +
  geom_point(aes(x = Longitude, y = Latitude), 
             data = EPGT_latlong, fill = "red", alpha=0.65, 
             size = 6,shape=21)

dev.off()

#SoCal_Baja_inset
description1<-"SoCal-Baja_region"
SoCal_Baja <- c(-121, 22.5, -109, 35)
#bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat

myMap1 <- get_map(location=SoCal_Baja, 
                  source="google", maptype="satellite", crop=TRUE) 

pdf(paste(description1, "_Gmap", ".pdf",sep=""), width = 8, height = 8.5)
ggmap(myMap1) +
  geom_point(aes(x = Longitude, y = Latitude), 
             data = EPGT_latlong,fill = "red", alpha=0.65, 
             size = 16, shape=21)
dev.off()

#Cen-Islands_inset
description2<-"Islands_region"
Islands_region <- c(-100, -10, -75, 11)
#bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat

myMap2 <- get_map(location=Islands_region, 
                  source="google", maptype="satellite", crop=TRUE) 

pdf(paste(description2, "_Gmap", ".pdf",sep=""), width = 8, height = 8.5)
ggmap(myMap2) +
  geom_point(aes(x = Longitude, y = Latitude), 
             data = EPGT_latlong, fill = "red", alpha=0.65,  
             size = 16, shape=21)
dev.off()

#So America Region Map
description3<-"SoAm_Region"
SoAm_Region <- c(-81, -25, -68,-13)
#bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat

myMap3 <- get_map(location=SoAm_Region, 
                  source="google", maptype="satellite", crop=TRUE) 

pdf(paste(description3, "_Gmap", ".pdf",sep=""), width = 8, height = 8.5)
ggmap(myMap3) +
  geom_point(aes(x = Longitude, y = Latitude), 
             data = EPGT_latlong, fill = "red", alpha=0.65, 
             size = 16,shape=21)

dev.off()
