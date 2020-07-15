#Seperate by site, colored by region####
region_color<-c("firebrick","forestgreen","deepskyblue3","purple","orange")
for (j in unique(group)){
pdf(paste(names(spx[j]),".pdf"), width=10, height=6)
palette(region_color)  
plot(spx[[j]],spy[[j]],type="p",ylim = c(6.5, 22),xlim = c(-26,-8.0))
legend("topright",legend=as.character(paste(names(spx[j]))))
SE <- standard.ellipse(spx[[j]],spy[[j]],steps=1)
SEA[j] <- SE$SEA
SEAc[j] <- SE$SEAc
palette(region_color) 
lines(SE$xSEAc,SE$ySEAc,col=spR[[j]],lty=1,lwd=3)
CH <- convexhull(spx[[j]],spy[[j]])
TA[j] <- CH$TA
palette(region_color) 
lines(CH$xcoords,CH$ycoords,lwd=3,lty=3,col=spR[[j]])
dev.off()
}

#Seperate by region, color groups within each####
datas<-data_new[,c(3,14,13,6)]
names(datas)<-c("group","x","y","region")
levels(datas$region)
SCBC<-subset(datas, region =="SC-BC Pac Coast")


# now loop through the data and calculate the ellipses
ngroups <- length(unique(SCBC$group))

# split the isotope data based on group
SCBC_spx <- split(SCBC$x,SCBC$group)
SCBC_spy <- split(SCBC$y,SCBC$group)
palette<-c("firebrick","forestgreen","deepskyblue3","purple","orange")

# create some empty vectors for recording our metrics
SEA <- numeric(ngroups)
SEAc <- numeric(ngroups)
TA <- numeric(ngroups)

pdf("SCBC.pdf", width=10, height=6)
plot(SCBC$x,SCBC$y,col=SCBC$group,type="p",ylim = c(6.5, 22),xlim = c(-26,-8.0))
legend("topright",legend=as.character(paste("Group ",unique(SCBC$group))),
       pch=19,col=1:length(unique(SCBC$group)))

for (j in unique(SCBC$group)){
 SE <- standard.ellipse(spx[[j]],spy[[j]],steps=1)
  SEA[j] <- SE$SEA
  SEAc[j] <- SE$SEAc
 lines(SE$xSEAc,SE$ySEAc,col="gray",lty=1,lwd=3)
 CH <- convexhull(spx[[j]],spy[[j]])
  TA[j] <- CH$TA
 lines(CH$xcoords,CH$ycoords,lwd=1,lty=3)
}
dev.off()

####EPac
EPac<-subset(datas, region =="EPac Islands")
ngroups <- length(unique(EPac$group))
EPac_spx <- split(EPac$x,EPac$group)
EPac_spy <- split(EPac$y,EPac$group)
SEA <- numeric(ngroups)
SEAc <- numeric(ngroups)
TA <- numeric(ngroups)

pdf("EPac.pdf", width=10, height=6)
plot(EPac$x,EPac$y,col=EPac$group,type="p",ylim = c(6.5, 22),xlim = c(-26,-8.0))
legend("topright",legend=as.character(paste("Group ",unique(EPac$group))),
       pch=19,col=1:length(unique(EPac$group)))
for (j in unique(EPac$group)){
  SE <- standard.ellipse(spx[[j]],spy[[j]],steps=1)
  SEA[j] <- SE$SEA
  SEAc[j] <- SE$SEAc
  lines(SE$xSEAc,SE$ySEAc,col="gray",lty=1,lwd=3)
  CH <- convexhull(spx[[j]],spy[[j]])
  TA[j] <- CH$TA
  lines(CH$xcoords,CH$ycoords,lwd=1,lty=3)
}
dev.off()

###GoC
GoC<-subset(datas, region =="Gulf of Cal")
ngroups <- length(unique(GoC$group))
GoC_spx <- split(GoC$x,GoC$group)
GoC_spy <- split(GoC$y,GoC$group)
SEA <- numeric(ngroups)
SEAc <- numeric(ngroups)
TA <- numeric(ngroups)

pdf("GoC.pdf", width=10, height=6)
plot(GoC$x,GoC$y,col=GoC$group,type="p",ylim = c(6.5, 22),xlim = c(-26,-8.0))
legend("topright",legend=as.character(paste("Group ",unique(GoC$group))),
       pch=19,col=1:length(unique(GoC$group)))

GoC<-for (j in unique(GoC$group)){
SE <- standard.ellipse(spx[[j]],spy[[j]],steps=1)
  SEA[j] <- SE$SEA
  SEAc[j] <- SE$SEAc
  lines(SE$xSEAc,SE$ySEAc,col="gray",lty=1,lwd=3)
  CH <- convexhull(spx[[j]],spy[[j]])
  TA[j] <- CH$TA
  lines(CH$xcoords,CH$ycoords,lwd=1,lty=3)
}
dev.off()
###Central-South America
CenSoA<-subset(datas, region =="Cen-SoAm Pac Coast")
ngroups <- length(unique(CenSoA$group))
CenSoA_spx <- split(CenSoA$x,CenSoA$group)
CenSoA_spy <- split(CenSoA$y,CenSoA$group)
palette<-c("firebrick","forestgreen","deepskyblue3","purple","orange")
SEA <- numeric(ngroups)
SEAc <- numeric(ngroups)
TA <- numeric(ngroups)

pdf("Central-South America.pdf", width=10, height=6)
plot(CenSoA$x,CenSoA$y,col=CenSoA$group,type="p",ylim = c(6.5, 22),xlim = c(-26,-8.0))
legend("topright",legend=as.character(paste("Group ",unique(CenSoA$group))),
       pch=19,col=1:length(unique(CenSoA$group)))
for (j in unique(CenSoA$group)){
  SE <- standard.ellipse(spx[[j]],spy[[j]],steps=1)
  SEA[j] <- SE$SEA
  SEAc[j] <- SE$SEAc
  lines(SE$xSEAc,SE$ySEAc,col="gray", lty=1,lwd=3)
  CH <- convexhull(spx[[j]],spy[[j]])
 TA[j] <- CH$TA
   lines(CH$xcoords,CH$ycoords,lwd=1,lty=3)
}
dev.off()