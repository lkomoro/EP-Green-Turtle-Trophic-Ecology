library(dplyr)
library(tidyr)
library(here)

data<-read.csv("./Files_local_repo_only/data/CM Isotope Data UPDATED_Jan122017.csv")
#updated 12-20-16 summary files reading in here to reflect new groupings, excluded data etc.
data$Ordered_SITE<-factor(data$Ordered_SITE, levels =c("1-SGR_SBN","3-SDB","4-NGU","6-BMA","7-IPD","8-LOR","9-BLA","11-CIN","12-NAV", "13-DUL","14-PAR","15-MEJ","16-ISL","17-COC","18-GOR","19-IGP","20-IGE","21-IGD","22-IGZ","23-IGN","24-PPE"))

#12-19-16 added: removing problematic sites and questionable values
data_new<-subset(data, Ordered_SITE!="7-IPD" &Ordered_SITE!="8-LOR"&Ordered_SITE!="16-ISL"&Ordered_SITE!="22-IGZ"&Ordered_SITE!="23-IGN")
data_new<-subset(data_new, Percent_N>=5 | is.na(data_new$Percent_N) )
data_new<-subset(data_new, Percent_N<=20 | is.na(data_new$Percent_N) )

data_new$yr<-gsub(".*/","",data_new$Collect_Date)
data_new$century<-20
data_new$colyr<-do.call(paste, c(data_new[c("century","yr")], sep = ""))


str(data_new)
N<-tapply(data_new$LABID, data_new$Location_Label,length)

data_new$Location_Label<-factor(data_new$Location_Label)
CCL_combined_mean<-tapply(data_new$CCL_combined, data_new$Location_Label,mean,na.rm=TRUE)
CCL_combined_min<-tapply(data_new$CCL_combined, data_new$Location_Label,min,na.rm=TRUE)
CCL_combined_max<-tapply(data_new$CCL_combined, data_new$Location_Label,max,na.rm=TRUE)

colyr_min<-tapply(data_new$colyr, data_new$Location_Label,min,na.rm=TRUE)
colyr_max<-tapply(data_new$colyr, data_new$Location_Label,max,na.rm=TRUE)

d15N_mean<-tapply(data_new$d15N, data_new$Location_Label,mean,na.rm=TRUE)
d15N_min<-tapply(data_new$d15N, data_new$Location_Label,min,na.rm=TRUE)
d15N_max<-tapply(data_new$d15N, data_new$Location_Label,max,na.rm=TRUE)

d13C_mean<-tapply(data_new$d13C, data_new$Location_Label,mean,na.rm=TRUE)
d13C_min<-tapply(data_new$d13C, data_new$Location_Label,min,na.rm=TRUE)
d13C_max<-tapply(data_new$d13C, data_new$Location_Label,max,na.rm=TRUE)

summary_by_location<-data.frame(N,CCL_combined_mean,CCL_combined_min,CCL_combined_max,colyr_min,colyr_max,d15N_mean,d15N_min,d15N_max,d13C_mean,d13C_min,d13C_max)
#write.table(summary_by_location,"SIA_data_summary",sep="/",row.names=TRUE) #Used for Table 1 in manuscript, with post-processing formatting


