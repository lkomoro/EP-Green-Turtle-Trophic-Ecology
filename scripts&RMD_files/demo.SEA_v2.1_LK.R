# this demo generates some random data for M consumers based on N samples and
# constructs a standard ellipse for each based on SEAc and SEA_B

rm(list = ls())
#library(devtools)
#install_github("andrewljackson/siar@v4.2.2", build_vingettes == TRUE)
library(siar)

data<-read.csv("CM Isotope Data UPDATED_Jan122017.csv")
#updated 12-20-16 summary files reading in here to reflect new groupings, excluded data etc.
data$Ordered_SITE<-factor(data$Ordered_SITE, levels =c("1-SGR_SBN","3-SDB","4-NGU","6-BMA","7-IPD","8-LOR","9-BLA","11-CIN","12-NAV", "13-DUL","14-PAR","15-MEJ","16-ISL","17-COC","18-GOR","19-IGP","20-IGE","21-IGD","22-IGZ","23-IGN","24-PPE"))
data_new<-subset(data, Ordered_SITE!="7-IPD" &Ordered_SITE!="8-LOR"&Ordered_SITE!="16-ISL"&Ordered_SITE!="22-IGZ"&Ordered_SITE!="23-IGN")
data_new$Ordered_SITE<-factor(data_new$Ordered_SITE)
data_new<-subset(data_new, Percent_N>=5 | is.na(data_new$Percent_N) )
data_new<-subset(data_new, Percent_N<=20 | is.na(data_new$Percent_N) )
datas<-data_new[,c(3,14,13,6)]
names(datas)<-c("group","x","y","region")
str(datas)
# ------------------------------------------------------------------------------
# ANDREW - REMOVE THESE LINES WHICH SHOULD BE REDUNDANT
# change this line
#setwd("...")

# ------------------------------------------------------------------------------

# now close all currently open windows
graphics.off()

# read in some data
# NB the column names have to be exactly, "group", "x", "y"
#mydata <- read.table("example_ellipse_data.txt",sep="\t",header=T)

# make the column names available for direct calling
attach(datas)

# now loop through the data and calculate the ellipses
ngroups <- length(unique(group))

# split the isotope data based on group
spx <- split(x,group)
spy <- split(y,group)

# create some empty vectors for recording our metrics
SEA <- numeric(ngroups)
SEAc <- numeric(ngroups)
TA <- numeric(ngroups)


for (j in unique(group)){
  
  
pdf(paste(names(spx[1]),".pdf"), width=10, height=6)
plot(spx[[1]],spy[[1]],type="p")
legend("topright",legend=as.character(paste(names(spx[1]))))
  # Fit a standard ellipse to the data
  SE <- standard.ellipse(spx[[1]],spy[[1]],steps=1)
  
  # Extract the estimated SEA and SEAc from this object
  SEA[1] <- SE$SEA
  SEAc[1] <- SE$SEAc
  
  # plot the standard ellipse with d.f. = 2 (i.e. SEAc)
  # These are plotted here as thick solid lines
  lines(SE$xSEAc,SE$ySEAc,col="forestgreen",lty=1,lwd=3)
    # Also, for comparison we can fit and plot the convex hull
  # the convex hull is plotted as dotted thin lines
  #
  # Calculate the convex hull for the jth group's isotope values
  # held in the objects created using split() called spx and spy
  CH <- convexhull(spx[[1]],spy[[1]])
  
  # Extract the area of the convex hull from this object
  TA[1] <- CH$TA
  
  # Plot the convex hull
  lines(CH$xcoords,CH$ycoords,lwd=3,lty=3,col="forestgreen")
dev.off()


# print the area metrics to screen for comparison
# NB if you are working with real data rather than simulated then you wont be
# able to calculate the population SEA (pop.SEA)
# If you do this enough times or for enough groups you will easily see the
# bias in SEA as an estimate of pop.SEA as compared to SEAc which is unbiased.
# Both measures are equally variable.
print(cbind(SEA,SEAc,TA))

# So far we have fitted the standard ellipses based on frequentist methods
# and calculated the relevant metrics (SEA and SEAc). Now we turn our attention
# to producing a Bayesian estimate of the standard ellipse and its area SEA_B


reps <- 10^4 # the number of posterior draws to make

# Generate the Bayesian estimates for the SEA for each group using the 
# utility function siber.ellipses
SEA.B <- siber.ellipses(x,y,group,R=reps)

# ------------------------------------------------------------------------------
# Plot out some of the data and results
# ------------------------------------------------------------------------------


# Plot the credible intervals for the estimated ellipse areas now
# stored in the matrix SEA.B
dev.new()
siardensityplot(SEA.B,
  xlab="Group",ylab="Area (permil^2)",
  main="Different estimates of Standard Ellipse Area (SEA)")

# and now overlay the other metrics on the same plot for comparison
points(1:ngroups,SEAc,pch=15,col="red")
legend("topright",c("SEAc"),pch=c(15,17),col=c("red","blue"))

# ------------------------------------------------------------------------------
# Compare two ellipses for significant differences in SEA
# ------------------------------------------------------------------------------

# to test whether Group 1 SEA is smaller than Group 2...
# you need to calculate the proportion of G1 ellipses that are less 
# than G2

Pg1.lt.g2 <- sum( SEA.B[,1] < SEA.B[,2] ) / nrow(SEA.B)

# In this case, all the posterior ellipses for G1 are less than G2 so 
# we can conclude that G1 is smaller than G2 with p approx = 0, and 
# certainly p < 0.0001.

# and for G1 < G3
Pg1.lt.g3 <- sum( SEA.B[,1] < SEA.B[,3] ) / nrow(SEA.B)

# etc...


# ------------------------------------------------------------------------------
# To calculate the overlap between two ellipses you can use the following code
# NB: the degree of overlap is sensitive to the size of ellipse you 
# choose to draw around each group of data. However, regardless of the choice
# of ellipse, the extent of overlap will range from 0 to 1, with values closer
# to 1 representing more overlap. So, at worst it is a semi-quantitative 
# measure regardless of extent of the ellipse, but the finer detials and 
# magnitudes of the effect size will be sensitive to this choice.
#
# Additional coding will be required if you wish to calculate the overlap 
# between ellipses other than those described by SEA or SEAc. 
# ------------------------------------------------------------------------------

# The overlap between the SEAc for groups 1 and 3 is given by:

# Fit a standard ellipse to the data
# NB, I use a small step size to make sure i get more "round" ellipses,
# as this method is computatonal and based on the discretisation of the
# ellipse boundaries.

overlap.G1.G3 <- overlap(spx[[1]],spy[[1]],spx[[3]],spy[[3]],steps=1)

#-------------------------------------------------------------------------------
# you can also cacluate the overlap between two of the convex hulls,
# or indeed any polygon using the code that underlies the overlap() function.

# fit a hull to the Group 1 data
hullG1 <- convexhull(spx[[1]],spy[[1]])

# create a list object of the unique xy coordinates of the hull
# the first and last entries are coincident for plotting, so ignore the first...
# hence the code to subset [2:length(hullG1$xcoords)] 
h1 <- list( x = hullG1$xcoords[2:length(hullG1$xcoords)] , y = hullG1$ycoords[2:length(hullG1$xcoords)] )

# Do the same for the Group 3 data
hullG3 <- convexhull(spx[[3]],spy[[3]])
h3 <- list( x = hullG3$xcoords[2:length(hullG3$xcoords)] , y = hullG3$ycoords[2:length(hullG3$xcoords)] )

# and calculate the overlap using the function in spatstat package.
hull.overlap.G1.G3 <- overlap.xypolygon(h1,h3)
