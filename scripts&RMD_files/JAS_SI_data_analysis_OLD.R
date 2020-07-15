setwd("~/Dropbox/home-NOAAtransitionfiles/Seminoff_isotope_MS_transitionfiles")
getwd()

#Load required libraries
library(ggplot2)

#Read in data
data<-read.csv("CM Isotope Data UPDATED.csv")

#Coarse data checks to note obvious data structure problems, etc.:
View(data)
str(data)#all look like appropriate categories
head(data)
tail(data)#got rid of some NAs at end, now should be good
summary(data)
#N.B. 87 entries are missing percent N or C, but have the delta N and C-keep in mind for downstream analyses

#looks good for now but leaving in example code here for if need to find which cases are causing issues:
summary(data$CCL)
which(data$CCL=="Y")



#Distributions:
p<-ggplot(data, aes(x=d15N)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue")+theme_bw()

s<-ggplot(data, aes(x=d15N)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.1,
                 colour="black", fill="blue") +theme_bw()+
  geom_density(alpha=.4, fill="yellow")
t<-s+coord_cartesian(ylim = c(0,2))
t+facet_wrap( ~ SITE.CODE, ncol=4)#super ugly, density is fucking it up

p+facet_wrap( ~ SITE.CODE, ncol=4)
#density only:
a<-ggplot(data, aes(x=d15N)) + geom_density(alpha=.4, fill="blue")+theme_bw()+
geom_vline(aes(xintercept=mean(d15N, na.rm=T)),   # Ignore NA values for mean
           color="red", linetype="dashed", size=1)
a+facet_wrap( ~ SITE.CODE, ncol=4)
#boxplots:
#color.scheme<-insert 
ggplot(data, aes(x=SITE.CODE, y=d15N, fill=H_type)) + geom_boxplot() +theme_bw()+
  guides(fill=FALSE)+stat_summary(fun.y=mean, geom="point", shape=5, size=4)
