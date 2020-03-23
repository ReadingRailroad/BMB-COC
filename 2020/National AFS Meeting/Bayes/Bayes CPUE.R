# R code for Hierarchical CPUE analysis - carp and buffalo project.
setwd() # set file directory


packages<-function(x, repos="http://cran.r-project.org", ...){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x, repos=repos, ...)
    require(x,character.only=TRUE)
  }
}
# Working Directory and Packages

packages(rjags)
packages(mcmcplots)
packages(ggplot2)
packages(gridExtra)
packages(reshape2)
packages(tidyr)
packages(rcompanion)
packages(lubridate)

# trying to get a single CPUE frame from all data:
std.cpue<-read.csv("CPUE Bayes Data.csv",header=T)


########################################################################
#
#
#         JAGS Models PART ONE: BUFFALO
#
#             
#
########################################################################
# define model
std.cpue2<-droplevels(subset(std.cpue, Species == "BIB"))

cpue.m <- jags.model(
  'FirstAttempt.bug',
  data=list(logPop=std.cpue2$logPop, # log pouplation size and 
            sdlogPop=std.cpue2$sdlogPop, # log sd of population size from data (sdlogpop becomes Tau) 
            n=dim(std.cpue2)[1], # number of samples
            logCPUE=log(std.cpue2$CPUE+1), # log-transformed CPUE from data + 1 to fix ln(0) problem
            WaterTemp=std.cpue2$WaterTemp, # Water temp in celcius, subtracted by 10 in bug file
            SDI=std.cpue2$SDI, # shoreline development index
            LakeSize=std.cpue2$LakeSize, # lake size in hectares
            MaxDepth=std.cpue2$MaxDepth, # maximum depth in meters subtracted by 10 in bug file
            MeanDepth=std.cpue2$MeanDepth), # average depth in meters subtracted by 10 in bug file
  n.chains=3, # number of simulation sequences
  n.adapt=100) # tells jags how much effort to spend in finding right resampling method

coef(cpue.m)

# sample the MCMC chains 1000 times as burn-in, ignore the samples. this gets thrown away
update(cpue.m, 1000)

# we want to plot:
## curve of estimated catchabaility 
## uncertainty 
cpue.i <- coda.samples(cpue.m, # model
                       c('logcatch','b0','b1','b2','b3','b4','b5','sd'), # parameters we want back
                       1000) # number of samples

par(mar=c(3,3,3,3)+0.2, mgp=c(2,0.8,0))
plot(cpue.i)

summary(cpue.i)


############################################
#
# we have some progress!
#
#
# beta 0: intercept
varnames(cpue.i) # reference variables in model
str(cpue.i) # structure of lists
beta0.out <- cbind(cpue.i[[1]][,1], cpue.i[[2]][,1], cpue.i[[3]][,1]) # subsetting for beta1 mcmc output
attributes(beta0.out) <- NULL # dropping attributes
dens.beta0 <- density(beta0.out) # density
q25.beta0 <- quantile(beta0.out, .025) # lower quantile
q975.beta0 <- quantile(beta0.out, .975) # upper quantile
dd.beta0 <- with(dens.beta0, data.frame(x,y)) # density dataframe

beta0<-qplot(x, y, data = dd.beta0, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta0, x>q25.beta0 & x<q975.beta0),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 0 - Intercept")+
  ylab("Density of Beta 0")+
  ggtitle("Buffalo Lakes: Intercept")
beta0

beta1.out <- cbind(cpue.i[[1]][,2], cpue.i[[2]][,2], cpue.i[[3]][,2]) # subsetting for beta1 mcmc output
attributes(beta1.out) <- NULL # dropping attributes
dens.beta1 <- density(beta1.out) # density
q25.beta1 <- quantile(beta1.out, .025) # lower quantile
q975.beta1 <- quantile(beta1.out, .975) # upper quantile
dd.beta1 <- with(dens.beta1, data.frame(x,y)) # density dataframe

beta1<-qplot(x, y, data = dd.beta1, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta1, x>q25.beta1 & x<q975.beta1),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 1 - Lake Size")+
  ylab("Density of Beta 1")+
  ggtitle("Buffalo Lakes: Lake Size")
beta1


beta2.out <- cbind(cpue.i[[1]][,3], cpue.i[[2]][,3], cpue.i[[3]][,3]) # subsetting for beta2 mcmc output
attributes(beta2.out) <- NULL # dropping attributes
dens.beta2 <- density(beta2.out) # density
q25.beta2 <- quantile(beta2.out, .025) # lower quantile
q975.beta2 <- quantile(beta2.out, .975) # upper quantile
dd.beta2 <- with(dens.beta2, data.frame(x,y)) # density dataframe

beta2<-qplot(x, y, data = dd.beta2, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta2, x>q25.beta2 & x<q975.beta2),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 2 - Maximum Depth (meters)")+
  ylab("Density of Beta 2")+
  ggtitle("Buffalo Lakes: Maximum Depth")
beta2


beta3.out <- cbind(cpue.i[[1]][,4], cpue.i[[2]][,4], cpue.i[[3]][,4]) # subsetting for beta3 mcmc output
attributes(beta3.out) <- NULL # dropping attributes
dens.beta3 <- density(beta3.out) # density
q25.beta3 <- quantile(beta3.out, .025) # lower quantile
q975.beta3 <- quantile(beta3.out, .975) # upper quantile
dd.beta3 <- with(dens.beta3, data.frame(x,y)) # density dataframe

beta3<-qplot(x, y, data = dd.beta3, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta3, x>q25.beta3 & x<q975.beta3),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 3 - Mean Depth")+
  ylab("Density of Beta 3")+
  ggtitle("Buffalo Lakes: Mean Depth")
beta3


beta4.out <- cbind(cpue.i[[1]][,5], cpue.i[[2]][,5], cpue.i[[3]][,5]) # subsetting for beta4 mcmc output
attributes(beta4.out) <- NULL # dropping attributes
dens.beta4 <- density(beta4.out) # density
q25.beta4 <- quantile(beta4.out, .025) # lower quantile
q975.beta4 <- quantile(beta4.out, .975) # upper quantile
dd.beta4 <- with(dens.beta4, data.frame(x,y)) # density dataframe

beta4<-qplot(x, y, data = dd.beta4, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta4, x>q25.beta4 & x<q975.beta4),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 4 - Shoreline Development Index")+
  ylab("Density of Beta 4")+
  ggtitle("Buffalo Lakes: SDI")
beta4


beta5.out <- cbind(cpue.i[[1]][,6], cpue.i[[2]][,6], cpue.i[[3]][,6]) # subsetting for beta5 mcmc output
attributes(beta5.out) <- NULL # dropping attributes
dens.beta5 <- density(beta5.out) # density
q25.beta5 <- quantile(beta5.out, .025) # lower quantile
q975.beta5 <- quantile(beta5.out, .975) # upper quantile
dd.beta5 <- with(dens.beta5, data.frame(x,y)) # density dataframe

beta5<-qplot(x, y, data = dd.beta5, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta5, x>q25.beta5 & x<q975.beta5),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 5 - Water Temp")+
  ylab("Density of Beta 5")+
  ggtitle("Buffalo Lakes: Water Temp")
beta5







########################################################################
#
#
#         JAGS Models PART TWO: CARP
#
#             
#
########################################################################
# define model
std.cpue2<-droplevels(subset(std.cpue, Species == "COC"))

cpue.m <- jags.model(
  'FirstAttempt.bug',
  data=list(logPop=std.cpue2$logPop, # log pouplation size and 
            sdlogPop=std.cpue2$sdlogPop, # log sd of population size from data (sdlogpop becomes Tau) 
            n=dim(std.cpue2)[1], # number of samples
            logCPUE=log(std.cpue2$CPUE+1), # log-transformed CPUE from data + 1 to fix ln(0) problem
            WaterTemp=std.cpue2$WaterTemp, # Water temp in celcius, subtracted by 10 in bug file
            SDI=std.cpue2$SDI, # shoreline development index
            LakeSize=std.cpue2$LakeSize, # lake size in hectares
            MaxDepth=std.cpue2$MaxDepth, # maximum depth in meters subtracted by 10 in bug file
            MeanDepth=std.cpue2$MeanDepth), # average depth in meters subtracted by 10 in bug file
  n.chains=3, # number of simulation sequences
  n.adapt=100) # tells jags how much effort to spend in finding right resampling method

coef(cpue.m)

# sample the MCMC chains 1000 times as burn-in, ignore the samples. this gets thrown away
update(cpue.m, 1000)

# we want to plot:
## curve of estimated catchabaility 
## uncertainty 
cpue.i <- coda.samples(cpue.m, # model
                       c('logcatch','b0','b1','b2','b3','b4','b5','sd'), # parameters we want back
                       1000) # number of samples

par(mar=c(3,3,3,3)+0.2, mgp=c(2,0.8,0))
plot(cpue.i)

summary(cpue.i)


############################################
#
# we have some progress!
#
#
# beta 0: intercept
varnames(cpue.i) # reference variables in model
str(cpue.i) # structure of lists
beta0.out <- cbind(cpue.i[[1]][,1], cpue.i[[2]][,1], cpue.i[[3]][,1]) # subsetting for beta1 mcmc output
attributes(beta0.out) <- NULL # dropping attributes
dens.beta0 <- density(beta0.out) # density
q25.beta0 <- quantile(beta0.out, .025) # lower quantile
q975.beta0 <- quantile(beta0.out, .975) # upper quantile
dd.beta0 <- with(dens.beta0, data.frame(x,y)) # density dataframe

beta0<-qplot(x, y, data = dd.beta0, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta0, x>q25.beta0 & x<q975.beta0),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 0 - Intercept")+
  ylab("Density of Beta 0")+
  ggtitle("Carp Lakes: Intercept")
beta0

beta1.out <- cbind(cpue.i[[1]][,2], cpue.i[[2]][,2], cpue.i[[3]][,2]) # subsetting for beta1 mcmc output
attributes(beta1.out) <- NULL # dropping attributes
dens.beta1 <- density(beta1.out) # density
q25.beta1 <- quantile(beta1.out, .025) # lower quantile
q975.beta1 <- quantile(beta1.out, .975) # upper quantile
dd.beta1 <- with(dens.beta1, data.frame(x,y)) # density dataframe

beta1<-qplot(x, y, data = dd.beta1, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta1, x>q25.beta1 & x<q975.beta1),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 1 - Lake Size")+
  ylab("Density of Beta 1")+
  ggtitle("Carp Lakes: Lake Size")
beta1


beta2.out <- cbind(cpue.i[[1]][,3], cpue.i[[2]][,3], cpue.i[[3]][,3]) # subsetting for beta2 mcmc output
attributes(beta2.out) <- NULL # dropping attributes
dens.beta2 <- density(beta2.out) # density
q25.beta2 <- quantile(beta2.out, .025) # lower quantile
q975.beta2 <- quantile(beta2.out, .975) # upper quantile
dd.beta2 <- with(dens.beta2, data.frame(x,y)) # density dataframe

beta2<-qplot(x, y, data = dd.beta2, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta2, x>q25.beta2 & x<q975.beta2),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 2 - Maximum Depth (meters)")+
  ylab("Density of Beta 2")+
  ggtitle("Carp Lakes: Maximum Depth")
beta2


beta3.out <- cbind(cpue.i[[1]][,4], cpue.i[[2]][,4], cpue.i[[3]][,4]) # subsetting for beta3 mcmc output
attributes(beta3.out) <- NULL # dropping attributes
dens.beta3 <- density(beta3.out) # density
q25.beta3 <- quantile(beta3.out, .025) # lower quantile
q975.beta3 <- quantile(beta3.out, .975) # upper quantile
dd.beta3 <- with(dens.beta3, data.frame(x,y)) # density dataframe

beta3<-qplot(x, y, data = dd.beta3, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta3, x>q25.beta3 & x<q975.beta3),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 3 - Mean Depth")+
  ylab("Density of Beta 3")+
  ggtitle("Carp Lakes: Mean Depth")
beta3


beta4.out <- cbind(cpue.i[[1]][,5], cpue.i[[2]][,5], cpue.i[[3]][,5]) # subsetting for beta4 mcmc output
attributes(beta4.out) <- NULL # dropping attributes
dens.beta4 <- density(beta4.out) # density
q25.beta4 <- quantile(beta4.out, .025) # lower quantile
q975.beta4 <- quantile(beta4.out, .975) # upper quantile
dd.beta4 <- with(dens.beta4, data.frame(x,y)) # density dataframe

beta4<-qplot(x, y, data = dd.beta4, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta4, x>q25.beta4 & x<q975.beta4),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 4 - Shoreline Development Index")+
  ylab("Density of Beta 4")+
  ggtitle("Carp Lakes: SDI")
beta4


beta5.out <- cbind(cpue.i[[1]][,6], cpue.i[[2]][,6], cpue.i[[3]][,6]) # subsetting for beta5 mcmc output
attributes(beta5.out) <- NULL # dropping attributes
dens.beta5 <- density(beta5.out) # density
q25.beta5 <- quantile(beta5.out, .025) # lower quantile
q975.beta5 <- quantile(beta5.out, .975) # upper quantile
dd.beta5 <- with(dens.beta5, data.frame(x,y)) # density dataframe

beta5<-qplot(x, y, data = dd.beta5, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta5, x>q25.beta5 & x<q975.beta5),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Beta 5 - Water Temp")+
  ylab("Density of Beta 5")+
  ggtitle("Carp Lakes: Water Temp")
beta5
























###########################################################




#    DO NOT RUN BELOW THIS: OLD CODE NOT UPDATED YET


###############################################################
# plot of distribution of ElogCPUE in boxplots, overlaid with observed CPUE 
cpue.4 <- coda.samples(cpue.m, 
                       c('ElogCPUE'), # parameters we want back
                       5000) # number of samples
summary(cpue.4)
# storing model output as object
cpue.4.out<-summary(cpue.4)

cpue.plot.data.carp<-cbind(carp,cpue.4.out[[1]][c(1:15),],cpue.4.out[[2]][c(1:15),c(1:5)])
str(cpue.plot.data.carp)

Carp.EvO.CPUE<-ggplot(cpue.plot.data.carp, aes(x = Date, y = log(CPUE)))+
  geom_boxplot(aes(ymin = cpue.plot.data.carp$`2.5%`, lower = cpue.plot.data.carp$`25%`, 
                   middle = cpue.plot.data.carp$Mean, 
                   upper = cpue.plot.data.carp$`75%`, ymax = cpue.plot.data.carp$`97.5%`),
               stat = "identity", fill = "darkgoldenrod")+
  geom_point(size=5)+
  theme_classic()+
  theme(axis.text=element_text(size=8),
        axis.text.x = element_text(angle=90),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Date")+
  ylab("Expected and observed ln (CPUE)")+
  ggtitle("Comparison of Expected Ln Carp CPUE Distribution vs. Observed Ln Carp CPUE")+
  ylim(0,5)
Carp.EvO.CPUE
########################################################################################