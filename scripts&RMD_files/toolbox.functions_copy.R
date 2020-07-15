#Building summarySE function to be able to summarize data to then make bar and line graphs####
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

geo_mean <- function(data) {
  log_data <- log(data)
  gm <- exp(mean(log_data[is.finite(log_data)]))
  return(gm)
}

#FUNCTIONS: create function panel.cor for correlations and panel.hist for histograms on SPLOMS####
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

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model)
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

#Coldiss function for gclus package-dissimilarity matrices etc.
"coldiss" <- function(D, nc = 4, byrank = TRUE, diag = FALSE)
{
  require(gclus)
  
  if (max(D)>1) D <- D/max(D)
  
  if (byrank) {
    spe.color = dmat.color(1-D, cm.colors(nc))
  }
  else {
    spe.color = dmat.color(1-D, byrank=FALSE, cm.colors(nc))
  }
  
  spe.o = order.single(1-D)
  speo.color = spe.color[spe.o,spe.o]
  
  op = par(mfrow=c(1,2), pty="s")
  
  if (diag) {
    plotcolors(spe.color, rlabels=attributes(D)$Labels, 
               main="Dissimilarity Matrix", 
               dlabels=attributes(D)$Labels)
    plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
               main="Ordered Dissimilarity Matrix", 
               dlabels=attributes(D)$Labels[spe.o])
  }
  else {
    plotcolors(spe.color, rlabels=attributes(D)$Labels, 
               main="Dissimilarity Matrix")
    plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
               main="Ordered Dissimilarity Matrix")
  }
  
  par(op)
}

## panelutils.R####
#
# License: GPL-2
# Author: Francois Gillet, February 2007
#
## Put Pearson, Spearman or Kendall correlations on the upper panel
panel.cor <- function(x, y, method="pearson", digits=3, cex.cor=1.2)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, method=method)
  ra <- cor.test(x, y, method=method)$p.value
  txt <- round(r, digits)
  sig <- 1
  prefix <- ""
  if(ra <= 0.1) prefix <- "."
  if(ra <= 0.05) prefix <- "*"
  if(ra <= 0.01) prefix <- "**"
  if(ra <= 0.001) prefix <- "***"
  if(ra <= 0.001) sig <- 2
  color <- 2
  if(r < 0) color <- 4
  # color <- "gray10"
  # if(r < 0) color <- "gray50"
  txt <- paste(txt, prefix, sep="\n")
  text(0.5, 0.5, txt, cex = cex.cor, font=sig, col=color)
}

## Put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
  # rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)
}


#Usage:
#pairs(num.mat, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist)
#pairs(num.mat, lower.panel=panel.smooth, upper.panel=panel.cor, method="kendall")
  
####################
## used in ecoPCA ##
####################

# evplot()
# Plot eigenvalues and percentages of variation
# Kaiser rule and broken stick model
#
# License: GPL-2
# Author: Francois Gillet, Octber 2007

evplot = function(ev) {
  # Broken stick model (MacArthur 1957)
  n = length(ev)
  bsm = data.frame(j=seq(1:n), p=0)
  bsm$p[1] = 1/n
  for (i in 2:n) {
    bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))
  }
  bsm$p = 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op = par(mfrow=c(1,2))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=T,
          main="% variation", col=c("bisque",2), las=2)
  legend("topright", c("% eigenvalue", "Broken stick model"),
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}

####################
## used in ecoPCA ##
####################

'cleanplot.pca' <- function(res.pca, ax1=1, ax2=2, point=FALSE, ahead=0.07, cex=0.7) {
  
  # A function to draw biplots from a PCA done with vegan.
  # res.pca: an object of class "rda" (PCA or RDA result from vegan)
  #
  # License: GPL-2
  # Authors: Francois Gillet & Daniel Borcard, April 2010
  
  require("vegan")
  
  # Two PCA biplots: scaling 1 and scaling 2
  # ****************************************
  
  par(mfrow=c(1,2))
  p <- length(res.pca$CA$eig)
  
  # Scaling 1: "species" scores scaled to relative eigenvalues
  sit.sc1 <- scores(res.pca, display="wa", scaling=1, choices=c(1:p))
  spe.sc1 <- scores(res.pca, display="sp", scaling=1, choices=c(1:p))
  plot.cca(res.pca, choices=c(ax1,ax2), display=c("wa","sp"), type="n",
           main="PCA - scaling 1", scaling=1)
  if (point) {
    points(sit.sc1[,ax1], sit.sc1[,ax2], pch=20)
    text(res.pca, display="wa", choices=c(ax1,ax2), cex=cex, pos=3, scaling=1)
  }
  else {
    text(res.pca, display="wa", choices=c(ax1,ax2), cex=cex, scaling=1)
  }
  text(res.pca, display="sp", choices=c(ax1,ax2), cex=cex, pos=4,
       col="red", scaling=1)
  arrows(0, 0, spe.sc1[,ax1], spe.sc1[,ax2], length=ahead, angle=20, col="red")
  pcacircle(res.pca)
  
  # Scaling 2: site scores scaled to relative eigenvalues
  sit.sc2 <- scores(res.pca, display="wa", choices=c(1:p))
  spe.sc2 <- scores(res.pca, display="sp", choices=c(1:p))
  plot(res.pca, choices=c(ax1,ax2), display=c("wa","sp"), type="n",
       main="PCA - scaling 2")
  if (point) {
    points(sit.sc2[,ax1], sit.sc2[,ax2], pch=20)
    text(res.pca, display="wa", choices=c(ax1,ax2), cex=cex, pos=3)
  }
  else {
    text(res.pca, display="wa", choices=c(ax1,ax2), cex=cex)
  }
  text(res.pca, display="sp", choices=c(ax1,ax2), cex=cex, pos=4, col="red")
  arrows(0, 0, spe.sc2[,ax1], spe.sc2[,ax2], length=ahead, angle=20, col="red")
}



'pcacircle' <- function (pca) {
  
  # Draws a circle of equilibrium contribution on a PCA plot
  # generated from a vegan analysis.
  # vegan uses special constants for its outputs, hence
  # the 'const' value below.
  
  eigenv <- pca$CA$eig
  p <- length(eigenv)
  n <- nrow(pca$CA$u)
  tot <- sum(eigenv)
  const <- ((n - 1) * tot)^0.25
  radius <- (2/p)^0.5
  radius <- radius * const
  symbols(0, 0, circles=radius, inches=FALSE, add=TRUE, fg=2)
}