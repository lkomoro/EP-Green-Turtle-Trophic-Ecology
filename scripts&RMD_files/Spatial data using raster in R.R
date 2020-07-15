#using raster package for biooracle data
library(raster)
setwd("c:/users/bscheng/Documents/MarineGEO/META/SST/Oracle")

#read in raster data
#asc is a file format

sst.mean<-raster("sstmean.asc")
sst.mean #like str for a raster
plot(sst.mean, main="SST") #plot raster
crs(sst.mean) #coordinate referencing system is not available
setMinMax(sst.mean) #calculate min and max sst in dataset, -1.7 and 32.9 C
hist(sst.mean, main="Mean SST Frequency Distribution", xlab="Temperature C", maxpixels=ncell(sst.mean)) #histogram of SSTs
ncell(sst.mean) #calls for number of cells in raster
nlayers(sst.mean) #number of raster layers

plot(sst.mean, main="SST", xlim=c(-95,-85),ylim=c(-5,5)) #plot raster
plot(sst.mean,main="Galapagos Islands", col=colorRampPalette(c("blue","purple", "yellow","red"))(255), 
     legend.args=list(text='Degrees C', side=4,font=2, line=2.5, cex=0.8), xlim=c(-95,-85), ylim=c(-5,5))
points(cbind(-90,0), pch=21, cex=1, bg="red") 
text(x=-88.5,y=0, labels="Shark Bait")
extract(x=sst.mean,y=cbind(-90,0), method="simple") 
extract(x=sst.mean,y=cbind(-90,0), method="bilinear") 


par.mean<-raster("parmean.asc")
plot(par.mean)

#extracting values - playing around with syntax
xy<-cbind(-150,0)
extract(x=sst.mean,y=xy, method="simple") #returns 26.5
xy<-cbind(0,-150)
extract(x=sst.mean,y=xy, method="simple") #returns NA, first column is longitude, second column is latitude

sites<-read.csv("oracle site sst extraction test.csv")

sites$sst.mean<-extract(x=sst.mean,y=cbind(sites$Longitude,sites$Latitude))
str(sites)
hist(sites$sst.mean)
plot(data=sites, LnRR.END~sst.mean)
summary(sites$sst.mean) #278 NAs, unsure what they are

#for lisa
plot(sst.mean,main="East Pacific", col=colorRampPalette(c("blue","purple", "yellow","red"))(255), 
     legend.args=list(text='Degrees C', side=4,font=2, line=2.5, cex=0.8), xlim=c(-152,-58), ylim=c(-50,50))
