setwd("C:/Users/bscheng/Dropbox/KomoCheng/Seminoff_isotope_MS_transitionfiles")

library(ggplot2); library(lme4); library(lmerTest)
data<-read.csv("CM Isotope Data UPDATED.csv")#note this is currently using updated file from Joel 10/27/16 with my edits that include the column edits and habitat type code
data2<-subset(data,Habitat_Type!="oceanic") #get rid of oceanic site before analysis
data_noNAs<-subset(data,Percent_N!="NA" )
length(which(data_noNAs$Percent_N<9)) 
length(which(data_noNAs$Percent_N>17))

str(data)
ggplot(data,aes(x=SITE_CODE,y=d15N))+geom_boxplot()
ggplot(data,aes(x=SITE_CODE,y=d15N))+geom_boxplot()+facet_grid(Habitat_Type~.)
ggplot(data,aes(x=Habitat_Type,y=d15N))+geom_boxplot()
ggplot(data,aes(x=SCL,y=CCL))+geom_point()
ggplot(data,aes(x=SCL,y=d15N, color=Habitat_Type))+geom_point()
ggplot(data2,aes(x=SCL,y=d15N,color=SITE_CODE))+geom_point()+facet_grid(Habitat_Type~.)
ggplot(data2,aes(SITE_CODE,y=SCL))+geom_boxplot()+facet_grid(Habitat_Type~.)
#oh I think you have a bunch of missing carapace data, for insular sites, only have 3 with carpace data but there are 5 sites according to frequency table below
table(data$SITE_CODE,data$Habitat_Type) #frequency table, note some sites have few samples (some 1s and 3s)

#analysis (don't read into results because some carapace data is missing)
#random effect of site nested within habitat type
m1<-lmer(data=data2, d15N~Habitat_Type+(1|Habitat_Type/SITE_CODE))      #single Habitat model
m2<-lmer(data=data2, d15N~CCL+(1|Habitat_Type/SITE_CODE))               #single CCL model
m3<-lmer(data=data2, d15N~Habitat_Type+CCL+(1|Habitat_Type/SITE_CODE))  #additive model
m4<-lmer(data=data2, d15N~Habitat_Type*CCL+(1|Habitat_Type/SITE_CODE))  #interactive model

summary(m1)
summary(m2)
summary(m3)
summary(m4)
