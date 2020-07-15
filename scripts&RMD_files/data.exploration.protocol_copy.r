#Data exploration in R general protocol-LK
#Created 8/6/14, currently is a conglomeration of a lot of different sources and needs to be cleaned upreorganized (increasingly so as go down the script...by the end I was just copying and pasting without any editing, so kind of a mess)
#but idea is to get things all in one place, then organize and make template that is easy to work from for different data sets
#Intro/load test wd and packages etc
source('/users/Komo/Desktop/Rfiles/toolbox.functions.R')
setwd('/users/Komo/Desktop/Rfiles/qPCR.analysis')
getwd()
require(ggplot2)

##NB-Lisa go back and actually combine my sections #1-4 with the analagous stuff in the graphic section from Andrew's class!***
#1. load in data (can use read.table or read.csv)
data4.wide.renamed<-read.csv ("Ct.all.normalized.wide.for.analysis.csv")
data4.wide.renamed$temp.treatment<-factor(data4.wide.renamed$temp.treatment, levels=c ("TC", "HC", "T1","T2","T3","T4","T5"))

#general data QC and exploration:
data<-data4.wide.renamed
data$recovery.time.no<-factor(data$recovery.time.no,levels=c ("0", "30", "60"))
data$relative.challenge.temp<-factor(data$relative.challenge.temp, levels=c ("TC", "HC", "CTMax-10","CTMax-8","CTMax-6","CTMax-4","CTMax-2", "CTMax-1"))


# general data QC and exploration:
#2. call for str () and summary() to check that all variables are the correctly read in ####
#and scan for weird things that indicate errors (e.g. ranges that are huge, NAs, etc.)
str(data)
#for all variables in df:
summary(data)#note a couple genes have NAs-go back and fix/check and/or make sure to include na.rm() commands when needed
summary(data$Ct.log2.form.HSP70)# target specific variables
#if data is not in df, but rather in vector, matrix, list etc., see r cookbook pg198
which(data$Ct.log2.form.CDKN1B<0)#if need to find cases of outliers, etc. based on summaries, histograms

quartz(4,4)#pull out graphic window

#subset (if needed) to be able to look at certain parts one at a time if complex dataset
adult<-subset(data, life.stage=="adult")
larval<-subset(data,life.stage=="30dph")

#3. Ggplot univariate graphic scans to look for normality, skew, and errors etc: (repeat for each response variable as needed)####
#don't need to do all three of these, depends on data/preference but saved code for all here in case needed

#histogram overall: note may want to play around with binning, etc. see R cookbook pg117-122
ggplot(data, aes(x=Ct.log2.form.HSP70)) + geom_histogram()
#density overall:
ggplot(data, aes(x=Ct.log2.form.HSP70)) + geom_density(alpha=.3)+
  ylab("Density")+
  xlab("HSP70")+
  theme(axis.text.x = element_text(size=15),axis.title.x = element_text(color="#000000", size = 20),
        axis.text.y = element_text(size=15),axis.title.y = element_text(color="#000000", size = 20))
#combined histogram/density (again, play around with binning etc. based on data):
ggplot(data, aes(x=Ct.log2.form.HSP70,y=..density..)) + geom_histogram(fill="cornsilk", colour="grey60",size=.2)+ 
  geom_density()+
  ylab("Density")+
  xlab("HSP70")

#dotplot (Wilkinson) overall:
ggplot(data, aes(x=Ct.log2.form.HSP70)) + geom_dotplot()

#Q-Q plot:
qqnorm(data$Ct.log2.form.HSP70)
qqline(data$Ct.log2.form.HSP70)#just adding line

#4 ggplot Bivariate combos to again check for errors, but also get sense for trends:####
#Cleveland dotplot by treatment:
ggplot(data, aes(x=Ct.log2.form.HSP70, y=reorder(temp.treatment,Ct.log2.form.HSP70))) +geom_point(size=3)

#density by treatment:
ggplot(data, aes(x=Ct.log2.form.HSP70, fill=temp.treatment)) + geom_density(alpha=.3)+
  ylab("Density")+
  xlab("HSP70")+
  theme(axis.text.x = element_text(size=15),axis.title.x = element_text(color="#000000", size = 20),
        axis.text.y = element_text(size=15),axis.title.y = element_text(color="#000000", size = 20))

#histogram by treatment:
ggplot(data, aes(x=Ct.log2.form.HSP70, fill=temp.treatment)) + geom_histogram()

#dotplot (Wilkinson) by treatment: (this really isnt that helpful bc overlaid, but maybe with small, well seperated datasets it is...I'd just use the Cleveland)
ggplot(data, aes(x=Ct.log2.form.HSP70, fill=temp.treatment)) + geom_dotplot()#can also swap axes if easier to view, see R grahpics cookbook pgs43-44 for more details

#Boxplot, adding in second predictor:(see R cookbook pgs 131-135 to add notches to assess median differences, markings for means, overlaid histograms, etc.)
ggplot(data,aes(x=temp.treatment,y=Ct.log2.form.HSP70))+
  geom_boxplot(aes(fill=recovery.time),position=position_dodge(.7))+
  theme_bw()+xlab("Temperature Treatment")+
  ylab("HSP70")+
  scale_fill_discrete(name="Recovery Time (min)")+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(axis.title.x=element_text(size=18))+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=18, angle=90))
#can do scatter if appropriate:(kind of weird for these data)
ggplot(data,aes(x=temp.treatment,y=Ct.log2.form.HSP70))+
  geom_point(aes(size=5,color=recovery.time),position=position_dodge(.7))+
  theme_bw()+xlab("Temperature Treatment")+
  ylab("HSP70")

#violin plot:
p<-ggplot(data,aes(x=temp.treatment,y=Ct.log2.form.HSP70))
p+geom_violin()#just basic, can adjust trimming and amount of smoothing etc, see r cookbook graphics pg136-137
p+geom_violin()+ geom_boxplot(width=.1,fill="black",outlier.colour="red")+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5)#with boxplot and median white dot overlaid

#playing around with other plots just to have the code, think about how to use with my data...####
#balloon plot-
balloon<-ggplot(data,aes(x=temp.treatment, y=recovery.time,size=Ct.log2.form.HSP70))+geom_point(shape=21,colour="black", fill="cornsilk")
#default is to map to radius, so will get a lot of small circles; add scale to area below to enlarge
balloon+scale_size_area(max_size=15)

#look for covariation between variables:
#Simple SPLOM-
pairs(data[,11:20])#my dataset actually has more genes, but just shortening for now bc otherwise panels too small
#customizing-
#FUNCTIONS: create function panel.cor for correlations and panel.hist for histograms ####
#(*lisa, you also put these functions into your toolbox so you can source it!)
panel.cor<-function(x,y,digits=2,prefix="",cex.cor,...) {
  usr<-par("usr")
  on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r<-abs(cor(x,y,use="complete.obs"))
txt<-format(c(r,0.123456789),digits=digits)[1]
txt<-paste(prefix,txt,sep="")
if(missing(cex.cor)) cex.cor<-0.8/strwidth(txt)
text(0.5,0.5,txt,cex=cex.cor*(1+r)/2)
}

panel.hist<-function(x,...) {
  usr<-par("usr")
  on.exit(par(usr))
  par(usr=c(usr[1:2],0,1.5))
  h<-hist(x,plot=FALSE)
  breaks<-h$breaks
  nB<-length(breaks)
  y<-h$counts
  y<-y/max(y)
  rect(breaks[-nB],0,breaks[-1],y, col="white",...)
}

panel.lm<-function(x,y,col=par("col"),bg=NA,pch=par("pch"),
                   cex=1,col.smooth="red",...) {
  points(x,y,pch=pch,col=col,bg=bg,cex=cex)
  abline(stats::lm(y~x), col=col.smooth,...)
}

#now can do different SPLOM types####
pairs(data[,11:20],upper.panel=panel.cor,
      diag.panel=panel.hist,
      lower.panel=panel.smooth)#default is lowess smoother

pairs(data[,11:20],pch=".",
      upper.panel=panel.cor,
      diag.panel=panel.hist,
      lower.panel=panel.lm)#change to linear regression line

#additional code and plots from Latimer class for data QC and exploration:####
library(MASS); library(lattice); library(vioplot)
# Check the data set is loaded and looks OK
head(data)

# First it's useful to look at the overall distribution of the data.
# That's good to know and also helps check for data entry errors.
hist(data$Ct.log2.form.HSP70)
stem(data$Ct.log2.form.HSP70)

# For example, what if someone typed "61.5" instead of "6.15"?
# Example of inserting the error into the data set (don't need to run this line of code, just as an example for errors, and if you wanted to change a value..could be useful in other ways too):
data$Ct.log2.form.HSP70[which(data$Ct.log2.form.HSP70==8.052995338)] = 80.52995338
# Now it's easy to see it if we just look:
stem(data$Ct.log2.form.HSP70)
# And if we're sure it's a typo we could fix it, or we could just discard that observation.
data$Ct.log2.form.HSP70[which(data$Ct.log2.form.HSP70==80.52995338)] = 8.052995338
stem(data$Ct.log2.form.HSP70)

# Quantile plots of the response variable
qqnorm(data$Ct.log2.form.HSP70, main="Normal Q-Q plot", xlab="Theoretical quantiles", ylab="Sample quantiles", )
# What does it mean if points in the lower tail are above the x=y or 1:1 line? 
# For example if the plot looked like this?
qqnorm(exp(data$Ct.log2.form.HSP70), main="Normal Q-Q plot", xlab="Theoretical quantiles", ylab="Sample quantiles")

#lisa-follow up to answer questions above...

# Looking at how the response variable Size is distributed within and across various subgroups of the data.
quartz(4,4)
# We can manually break the data into groups for display. Usually though we'd use automated ways, see trellis plots and box plots below. 
x1 = data$Ct.log2.form.HSP70[data$temp.treatment=="HC"]
x2 = data$Ct.log2.form.HSP70[data$temp.treatment=="T4"] #just doing 2 treatments for demostrative purposes-can go back and split further, etc.
vioplot(x1, x2, names=c("HC", "T4"), col="lightgray")
boxplot(Ct.log2.form.HSP70~temp.treatment, data, xlab="Temperature Challenge")
# Q: What are the different parts of the boxplot displaying and how might those be useful?

# Using boxplots to compare across different groups
boxplot(Ct.log2.form.HSP70~temp.treatment, data, ylab="HSP70 expression", xlab="Treatment", notch=T)
boxplot(Ct.log2.form.HSP70~temp.treatment+recovery.time, data, col=c("lightblue", "pink"), ylab="HSP70 expression", notch=T)

# Looking at relationships among variables in the data set. 
# Basic scatter plot (not really apply with my test dataset here bc categorical, defaults back to boxplot, but if had continuous predictors would be scatter)
plot(Ct.log2.form.HSP70~temp.treatment, data, ylab="HSP70 expression", xlab="Treatment") 

# Stripplot: For looking at continuous data against a factor or grouping variable, in addition to boxplots there are stripplots (also called dotplots):
stripplot(Ct.log2.form.HSP70~temp.treatment, data=data, jitter.data=T, group=recovery.time, aspect=1, xlab="Treatment")

# Lattice plots for displaying scatterplots of two factors stratified by a third ***these are helpful for me bc of second (and third) predictors!
# Q: What do these show you that the simple scatterplot doesn't? 
xyplot(Ct.log2.form.HSP70~temp.treatment|life.stage, data)
stripplot(Ct.log2.form.HSP70~temp.treatment|life.stage, data=data, jitter.data=T, aspect=1, xlab="Treatment")

# adding a smoothed trendline for visualization (Chambers et al. argue this is often key)
xyplot(size~Time|treat, Sitka, type=c("p", "smooth"))
#****Here do the three predictors with a response:
xyplot(Ct.log2.form.HSP70~temp.treatment|life.stage, data, group=recovery.time, type=c("p", "smooth"))
xyplot(Ct.log2.form.HSP70~temp.treatment|stage.accl.comb, data, group=recovery.time, type=c("p", "smooth"))
warnings()

#playing around with applying coplot to my data:
coplot(Ct.log2.form.HSP70~temp.treatment|life.stage,data)#maybe useful but not really different from lattice-better to use this if need to look at 'moving window' as explained below
#but this might be useful if think relationship changes over span of x...think about maybe applying with different recovery temps??...#coplot I think is good to look for interactions...

#Latimer class 2 script amended for data exploration and analysis:####
library(lattice); library(lme4); library(arm)
# Here is another data set. At a set of sites in South Africa, a field crew measured the number of live and dead shrubs of the widespread species Protea repens. 
setwd('/users/Komo/Desktop/Rfiles')
getwd()
d = read.table("protea_survival.txt", header=T)
# We're interested in how this species will respond to climate change. As in many Mediterranean-climate regions temperature is projected to increase, 
#while precipitation projections are inconsistent but suggest a drying trend is likely. 
# Each row in the data set represents one site where a population of the species has been observed. 
#At each site, we have some environmental observations (max summer temperature "max01", mean annual precipitation "map", and the seasonality of precipitation "rainconc"). 
# At only a subset of sites, we have information on the response variable: mortality rate over the previous year ("perc.alive"). 
# We would like to use these data to learn something about the relationship between temperature and precipitation on the one hand, 
#and mortality on the other. Use graphical techniques to explore to what extent we might be able to do this, and to identify weaknesses. 

#A few functions that might be useful in that exercise that are not in the script are:
is.sampled = as.integer(!is.na(d$perc.alive)) #I think this is taking essentially splitting up the observations for which we have perc.alive data vs NAs, and making it an integer, so then can split up in future plots/analyses 
#(e.g. color coded like plot below, or in coplot)
plot(map~max01, d, col=is.sampled+3, pch=16)#this plots max summer temperature "max01" vs. mean annual precipitation (map); pch=the shape, col=I think this is setting different colors for the 
#ones that have corresponding percent alive data or not
cor(d$map,d$max01)
pairs(d[,2:4])
#  use to view “moving window” of sub-sections of predictors-see example below where have multiple continuous variables...
coplot (perc.alive~map|rainconc,d,row=1,columns=6)

# Some particular questions:
# 1) To what extent can we use this data set to infer about mortality across the range of the species?
#  -Discussion points: 
#	can exclude those without the percent alive data and did correlative plot only with sub-dataset, seem to be most correlated with seasonality
#	one problem is that we didn’t sample across the whole range (no data at “hot” end of temp zone)
#	can come up a lot with observational studies…
# 2) Will we be able to make distict conclusions about precipition and temperature? if not, why not? What might improve our ability to do that? 
#	-Discussion:
#	problem of collinearity, need to do manipulative experiment if you want to tease apart the different effects
# 3) What it if turns out the species behaves very differently when the seasonality of precipitation shifts from one pattern to another (i.e. at different values of rainconc)? 
#To what extent can we take that into account, or nonetheless make inferences about responses to mean annual precipitation and summer maximum temperature? Explain...
#	-Discussion: 
#	use to view “moving window” of sub-sections of predictors-
coplot (perc.alive~map|rainconc,d,row=1,columns=6)
#	how do you choose, set up the bins if you don’t want to use the default?
#	he plotted “less seasonal” and more seasonal” and see trend in scatterplot (x=map, y=perc.alive)

#################################################################################
#  Regression (and linear models more generally)
#  to check model look at the fit and residuals
#	and also how the model predicts (predictive checks and cross validation, simulating from a model can also help improve the design)
#	Assumptions of linear regression :
#(see reading and link for more explanations, these are in order of importance according to Gelman, but may be different order for example if you were more interested in testing a hypothesis vs. prediction, i.e. testing hypothesis, maybe equal variance would be more important bc can influence p-value)
#	1. Validity
#	2. Additivity and linearity
#	3. Independence of errors
#	4. Equal variance of errors 
#	5. Normality of errors
#	What is a linear model: See powerpoint for graph
#	The only 2 sources of variation in regression model:
#	1. uncertainty about coefficients (slope and intercept estimates)-
#	this is encapsulated in the coef. st. errors (if plot estimated slope, it will often have the shading on each side that corresponds to its coef. st. error-showing how confident we are in that slope. may need to look into this more, because I think previously I was confusing this with the 95% CI (see below)
#	how confident we are about the relationship between the variables
#	2. Residual noise/error (scatter around the line)
#	assumed constant, independent normal
#	if put 95% CI around estimated line, expect that 95% of data is within, and if you make predictions based on the line how confident will you be in your estimate…
#	look at residual vs. fit, look for clumping, can color code by other variables to make sure not biased (see slide with species color coded)
#	See/read paper: Graefe (2013) Improving forecasts using equally weighted predictors (presidential elections) …talking about avoiding overfitting models by weighting (which limits predictive abilities)…better to include the relevant variables than to weight them…
#	Taps on large issues of 1) how complex to make the models “legitimately”…, but 2) it’s really important to include the relevant information

#examples and code to adapt:
# PART 1: Multiple regression -- why we might want or need to consider multiple variables
# a) explain more variation (when no interaction between variables but both are explanatory). 
# Example:Predicting the length of petals of Iris versicolor from other morphological measurements 
# In this case, excluding the additional relevant variable results in "underfit". 
plot(Petal.Length~Petal.Width, iris, subset=iris$Species=="versicolor")
plot(Petal.Length~Sepal.Length, iris, subset=iris$Species=="versicolor")
coplot(Petal.Length~Petal.Width|Sepal.Length, iris[iris$Species=="versicolor",])
summary(lm(Petal.Length~Petal.Width*Sepal.Length, iris, subset=iris$Species=="versicolor"))
# NB can use display() command after running lm model (instead of summary which give you a lot of extra stuff)
display(lm(Petal.Length~Petal.Width*Sepal.Length, iris, subset=iris$Species=="versicolor"))

# b) The association between the response and one variable depends on level of other variables.
#   This is what people often mean when they say an analysis "controlled for" other variables. 
#   They didn't really control for them in the experimental sense. Instead the analysis
#   is able to account for dependence in the relationship between the response and the 
#   explanatory variable of interest. 
proteas = read.table("protea_survival.txt", header=T)
coplot(perc.alive~map|rainconc, proteas)
summary(lm(perc.alive~map*rainconc, proteas))

# Total precip seems to matter only when rainfall concentration is low. 
# We can see this more clearly by splitting the data into "high" and "low" seasonality locations.
plot(perc.alive~map, proteas, subset=which(proteas$rainconc<median(proteas$rainconc)), col="blue", pch=16)
points(perc.alive~map, proteas, subset=which(proteas$rainconc>=median(proteas$rainconc)), col="red", pch=16)

# Q What's the interpretation of the regression coefficients here? 

# Q in what sense is this really "controlling" for? 
# What needs to be true in order for you to say it's controlling for the other variable?
# At a minimum, adequate sampling across the range of both... 
# where adequate means across the whole range of variables in which the response occurs. 

##########################################################
# PART 2: Regression fitting, diagnostics, interpretation 
# When you have a continuous and a categorical variable, the requirement to sample across the whole range equates to balance. 
# As we'll see, models with random effects are more robust to imbalance but there's no way around needing to have enough sample size to learn about each level of a factor. 

# (Note here is another reason you might choose to use a random effect -- you can have low sample size in some groups/categories, you just won't learn much about that particular one but you can still generalize across them). 
setwd('/users/Komo/Desktop/Rfiles')
getwd()
d = read.table("Erodium_data_for_class2.csv", sep=",", header=T)

# Data set explanation: 
# These data are from a greenhouse experiment in which widespread invasive plant Erodium cicutarium
# was planted in high and low-water treatments. This exercise explores the effect
# of plant phenology (how soon they flower) and watering treatment on plant size (stem length).
# treatment (1/0) = low-water treatment (1) or high-water treatment (0)
# stem_length = length of longest stem on each plant in cm
# days_to_flower = time from planting to flowering in days

# We can initially look at the effect of days to flower and treatment on plant size 

#red is low water treatment
plot(stem_length~days_to_flower, d, pch=16, col=(d$treatment=="low_water")+1)

#transforms it
plot(log(stem_length)~days_to_flower, d, pch=16, col=(d$treatment=="low_water")+1)

m1 = lm(log(stem_length)~days_to_flower*treatment, d)
summary(m1)
str(d)
# Or you might prefer the display() function in Gelman & Hill's arm library which is much cleaner:

display(m1)

# Here's a quick way to check a couple of assumptions of the linear model:
par(mfrow=c(2, 1))
plot(m1, which=1:2)

# But then how would you interpret the intercept and the effect of treatment?
# Does this give a sensible interpretation? 
# One reason to center the explanatory variables is to make the effects more interpretable. 

d.center = d
d.center$days_to_flower=d$days_to_flower-mean(d$days_to_flower, na.rm=T)      

# Then we can repeat the same regression and compare. 

m2 = lm(log(stem_length)~days_to_flower*treatment, d.center)
display(m2)
summary(m2)
plot(log(stem_length)~days_to_flower, d.center, pch=16, col=(d$treatment=="low_water")+1)

# Which coefficients change? Now what do the intercept and treament effects mean? 
#LK this allows you to look at the effect size/estimate intercept where the center of your data...where your data really is collected,
#instead of out at zero where you aren't really collecting data (likely)...so this helps you to 
#estimate effects where you are really interested in (you could also standardize to a specific x value that you are interested, etc.)
#note-this doesn't work for categorical variables obviously, unless you give them dummy # ids

# Q how would you compare the sizes of the effect of treatment and days to flower? 
# G&H recommend centering all variables, and dividing continuous variables by 2x their standard deviation. 
# This makes the effects of continuous variables more comparable to those of 1/0 categorical variables. 
# See G&H pp56-57.

# scaling can help when different variables have very different units, so it 
#can be hard to interpret the effect sizes between variables with very different units 
#(e.g. so it can make something look like it has a big effect size)
# rationale of making cont. variables comparable to 0/1 categorical variables?...Andrew explained but I'm still unclear on it...
# ...something about making direct comparisons of effect sizes of cont/cat. variables...

d.scaled = d.center
d.scaled$days_to_flower = d.center$days_to_flower/(2*sd(d.center$days_to_flower))

# Or we can do the same thing using the rescale() function in arm library
d.scaled$days_to_flower = rescale(d.center$days_to_flower)

m3 = lm(log(stem_length)~days_to_flower*treatment, d.scaled)
display(m3)

summary(m3)
summary(m2)
summary(m1)

######################################################
# PART 3: How can you tell if a model is good, or good enough?  

# a) Single-number summaries. 
# R^2 and other measures of fit or prediction 

# b) Fit 
# Plot fitted values vs observed.

# It can be helpful to plot the data with fitted lines. 

plot(log(stem_length)~days_to_flower, d.scaled, pch=16, col=(d$treatment=="low_water")+1)
betas = coef(m3) # this extracts the coefficients
abline(betas[1], betas[2], col=1) # regression line for treatment=0
abline(betas[1]+betas[3], betas[2]+betas[4], col=2) # regression line for treatment=1


# Plotting the regression line plus confidence intervals.
# Keeping it simple, let's use just one explanatory variable.

m4 = lm(log(stem_length)~days_to_flower, d.scaled)

x.pred = seq(min(d.scaled$days_to_flower)*1.1, max(d.scaled$days_to_flower)*1.1, by=0.05)
# set the range of the explanatory variable to use for displaying the regression. 
erodium.pred = predict(m4, data.frame(days_to_flower=x.pred), se.fit=TRUE, interval="prediction") 
# This lets us predict the response variable from various levels of the explanatory variable. 
# We can also get intervals around that prediction. We can get "predictive" interval, or a "confidence" interval (see below). 
plot(x.pred, erodium.pred$fit[,1], type="l", ylim=c(-3, 4), ylab="log(Stem Length mm)", xlab="Days to flower")
lines(x.pred, erodium.pred$fit[,2], lty=2)
lines(x.pred, erodium.pred$fit[,3], lty=2)
points(log(stem_length)~days_to_flower, d.scaled)

# Are there too many points outside the 95% prediction interval? 

# For comparison, here is a plot showing the confidence interval around the regression line:
erodium.pred = predict(m4, data.frame(days_to_flower=x.pred), se.fit=TRUE, interval="confidence") 
plot(x.pred, erodium.pred$fit[,1], type="l", ylim=c(-3, 4), ylab="log(Stem Length mm)", xlab="Days to flower")
lines(x.pred, erodium.pred$fit[,2], lty=2)
lines(x.pred, erodium.pred$fit[,3], lty=2)
points(log(stem_length)~days_to_flower, d.scaled)

# Now how would you interpret the interval around the best-fit regression line? 
# Can you simulate replicate data using the model? Remember what the regression model actually is, 
# as a probability distribution, and what the parameters are. 

###################################
#various exploratory and model diagnostic code pulled from my previous scripts-put here for now to collate, will need to go back and organize...
#note this also isn't an exhaustive list...right now a whole lot of overlap, etc., need to adjust with my one test dataset and clean up/re-organize!!

#note don't run the code below without adjusting-don't have code for input files here so won't correspond/will just return error messages...
#for mixed models:
chronic.thermal.adult.data<-read.csv("adult.dec.thermal.chronic.alive.dead.counts.csv")##first, use approach where I have counts of # alive and # dead at each temp and tank and cbind them, run the GLM and GLMM (with random effect of tank)
##note: not accounting for fish identity in this model
y<-cbind(chronic.thermal.adult.data$cum.dead,chronic.thermal.adult.data$alive)#Check with Brian this may be wrong syntax for cbind...i think need to adjust to counts vs. trials...
#note I didn't end up using these data/models in my paper bc needed to do expanded 1's and 0s to deal with overdispersion...
#Example temp GLMM with tank as random effect:
mm1<-glmer(y~chronic.thermal.adult.data$temp.c+(1|chronic.thermal.adult.data$tank), family=binomial)
overdisp_fun(mm1)#need to build this function before using-is in toolbox
##saving residuals and fitted values for graphing; can copy paste if want to look at other models too:
chronic.thermal.adult.data$mm1.resid<-resid(mm1)
chronic.thermal.adult.data$mm1.fitted<-fitted(mm1)

##plot fitted vs. residuals...this is the same as one of the crawley graphs but comes out a bit 
#cleaner to look for fanning, etc. to check assumption of homogeniety of variances:
plot(chronic.thermal.adult.data$mm1.resid~chronic.thermal.adult.data$mm1.fitted)

#Liklihood ratio tests to compare multiple mixed models:
anova(mm1a,mm0a,mm2a)

## odds ratios only
exp(coef(m1a))
## odds ratios and 95% CI
exp(cbind(OR = coef(m1a), confint(m1a)))

#comparing m1a vs. null:
#To find the difference in deviance for the two models (i.e., the test statistic) we can use the command:
with(m1a, null.deviance - deviance)
#The degrees of freedom for the difference between the two models is equal to the number of predictor variables in the mode, and can be obtained using:
with(m1a, df.null - df.residual)
#Finally, the p-value can be obtained using:
with(m1a, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) # p-value <0.05 here tells you that your model fits better than the null
#returns log liklihood of model:
logLik(m1a)

##
par(mfrow=c(1,1))
##saving residuals and fitted values for graphing/diagnostics:
chronic.thermal.adult.expanded.data$mm2a.resid<-resid(mm2a)
chronic.thermal.adult.expanded.data$mm2a.fitted<-fitted(mm2a)
##plot fitted vs. residuals...this is the same as one of the crawley graphs but comes out a bit cleaner to look for fanning, etc. to check assumption of homogeniety of variances
plot(chronic.thermal.adult.expanded.data$mm2a.resid~chronic.thermal.adult.expanded.data$mm2a.fitted)
##
##ANOVAs

##Oneway ANOVA:Medium acclimation Temps only, where testing if CTMax~Life Stage
##Rerun subset code just to check:
CTMax.med.only<-subset(CTMax.data,cat.temp=="medium"& test=="thermal")
Med.life.stage.aov.model1<-aov(CTMax.med.only$temp.C ~ CTMax.med.only$stage)
summary.aov(Med.life.stage.aov.model1)
TukeyHSD(Med.life.stage.aov.model1)

#changing to glm/lm format for contiunity in paper...same analysis results since both predictors are designated as factors...
Med.life.stage.lm.model1<-lm(temp.C ~ stage, data=CTMax.med.only)
summary(Med.life.stage.lm.model1)
summary.aov(Med.life.stage.lm.model1)
Anova(Med.life.stage.lm.model1)
#(using glht command in lieu of TukeyHSD bc won't work with lm models for pairwise comparisons, but results are almost identical)
# see: http://www.ats.ucla.edu/stat/r/faq/testing_contrasts.htm for more details on specifying what contrasts you want to run, etc.
#below is the command for running all pairwise comparisons:
l2<-glht(Med.life.stage.lm.model1,linfct=mcp(stage="Tukey"))
summary(l2)
#these commands give the plots that Brian (and I like) bc are easier to interpret:
par(mfrow=c(1,1))
##saving residuals and fitted values as variables into dataset so can graph
CTMax.med.only$stage.resid<-resid(Med.life.stage.lm.model1)
CTMax.med.only$stage.fitted<-fitted(Med.life.stage.lm.model1)
##plot fitted vs. residuals...this is the same as one of the crawley graphs but comes out a bit cleaner to look for fanning, etc. to check assumption of homogeniety of variances
plot(CTMax.med.only$stage.resid~CTMax.med.only$stage.fitted)
#since stage is a factor, easier to check equal variance assumption with boxplots of residuals for each group
plot(CTMax.med.only$stage.resid~CTMax.med.only$stage)
##this is a histogram of the residuals, which will tell you if you have normally distributed errors
ggplot(CTMax.med.only, aes(x=stage.resid)) + geom_histogram(binwidth=.3)

#(can only use tukey command with AOV command)
Med.life.stage.aov.model1<-aov(CTMax.med.only$temp.C ~ CTMax.med.only$stage)
summary(Med.life.stage.aov.model1)
TukeyHSD(Med.life.stage.aov.model1)

##Med.life.stage.aov.model1 diagnostics:
##these formal tests are unnecessary and not all that helpful but I ran them just to see because they were listed in Crawley
fligner.test(CTMax.med.only$temp.C~CTMax.med.only$stage)
bartlett.test(CTMax.med.only$temp.C~CTMax.med.only$stage)
##acceptable to evaluate meeting the assumptions graphically, and easier to try transformations and see if they help fix issues
##these two lines give the plots from Crawley, see explanation on pg. 508-509 if needed
par(mfrow=c(2,2))
plot(aov(CTMax.med.only$temp.C~CTMax.med.only$stage))
#these commands give the plots that Brian (and I like) bc are easier to interpret:
par(mfrow=c(1,1))
##saving residuals and fitted values as variables into dataset so can graph
CTMax.med.only$stage.resid<-resid(Med.life.stage.aov.model1)
CTMax.med.only$stage.fitted<-fitted(Med.life.stage.aov.model1)
##plot fitted vs. residuals...this is the same as one of the crawley graphs but comes out a bit cleaner to look for fanning, etc. to check assumption of homogeniety of variances
plot(CTMax.med.only$stage.resid~CTMax.med.only$stage.fitted)
#since stage is a factor, easier to check equal variance assumption with boxplots of residuals for each group
plot(CTMax.med.only$stage.resid~CTMax.med.only$stage)
##this is a histogram of the residuals, which will tell you if you have normally distributed errors
ggplot(CTMax.med.only, aes(x=stage.resid)) + geom_histogram(binwidth=.3)

##trying some transformations...below is squaring...that did nothing,literally nothing, to help. I also did a x^3 transformation but erased it because it was also worthless
CTMax.med.only$temp.Csq<-(CTMax.med.only$temp.C)^2
Med.life.stage.aov.model2<-aov(CTMax.med.only$temp.Csq ~ CTMax.med.only$stage)
summary.aov(Med.life.stage.aov.model2)
TukeyHSD(Med.life.stage.aov.model2)
##alternately, this is the R code for running the Welch test-corrects for df and doesn't have assumption of equal variances; 
#drawback is that you can't do post-hoc tests...
Med.life.stage.one.way.model1<-oneway.test(CTMax.med.only$temp.C ~ CTMax.med.only$stage)
Med.life.stage.one.way.model1

##Remember need to change numerical predictors like accl.temp or salinity into a factor if want to be able to do post-hoc tests