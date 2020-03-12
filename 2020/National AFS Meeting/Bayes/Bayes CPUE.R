# R code for Hierarchical CPUE analysis - carp and buffalo project.

packages<-function(x, repos="http://cran.r-project.org", ...){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x, repos=repos, ...)
    require(x,character.only=TRUE)
  }
}
# Working Directory and Packages
setwd("~/BMB-COC/2020/National AFS Meeting/Bayes/") # set to file directory for this analysis
packages(rjags)
packages(mcmcplots)
packages(ggplot2)
packages(gridExtra)
packages(reshape2)
packages(tidyr)
packages(rcompanion)
packages(lubridate)

# trying to get a single CPUE frame from all data:
all.data<-read.csv("2017-2020 Compiled Data.csv",header=T)
# however, no standard runs were used in 2017, so let's skip that ish
all.data<-droplevels(subset(all.data, Year == 2018 |
                                      Year == 2019))
levels(all.data$Site.Transect)

std.data<-droplevels(subset(all.data, Site.Transect == "Std 1" |
                                      Site.Transect == "Std 2" |
                                      Site.Transect == "Std 3" |
                                      Site.Transect == "Std 4" |
                                      Site.Transect == "Std 5" |
                                      Site.Transect == "Std 6" |
                                      Site.Transect == "Std 7" |
                                      Site.Transect == "Std 9" ))
# just carp and buff
levels(std.data$Species)
std.data<-droplevels(subset(std.data, Species == "BIB" |
                                      Species == "COC" |
                                      Species == "dummy"))

# adding an effort column, the on-time divided by an hour (3600 seconds)
std.data$Effort<-std.data$On.Time..s./3600

# need to set 'dummy' to zeros for carp and buff:
coc.extra<-droplevels(subset(std.data, Species == "dummy"))
coc.extra$Species<-as.character(coc.extra$Species)
coc.extra$Species[coc.extra$Species == "dummy"] <- "COC"
coc.extra$Species<-as.factor(coc.extra$Species)
coc.extra$detect<-0
head(coc.extra)
# will add on the coc.extra frame shortly. changing buff species and detect
std.data$detect[std.data$Species == "dummy"] <- 0
std.data$Species[std.data$Species == "dummy"] <- "BIB"
# adding on coc.extra
std.data<-rbind(std.data, coc.extra)

# first inklings of a CPUE frame
std.cpue<-aggregate(detect~Date+Lake+Site.Transect+Species+Effort+Water.Temp..C., 
                          data = std.data, sum, na.action=NULL)
head(std.cpue) # data frame of total captured in both species (and dummy)





# cpue column:
std.cpue$CPUE<-std.cpue$detect/std.cpue$Effort

# check data frame:
std.cpue<-droplevels(std.cpue)
str(std.cpue)

# OK, delete Effort and Detect columns to make things "easier" lol
# maybe cleaner is a more appropriate term
std.cpue<-std.cpue[,-c(5,7)]

# Now, I suppose that I could use the mean for each day, 
#                   which is what we did in class I think:

std.cpue2<-aggregate(CPUE~Date+Lake+Species+Water.Temp..C., 
                      data = std.cpue, mean, na.action=NULL)
# 4-28-2019 at Silver is all zeros, but that's accurate. Only fish that day was by the bridge

############################################################
#
#       Adding lake-specific covariates
#
#
levels(std.cpue2$Lake)
# Surface area (hectares)
std.cpue2$LakeSize[std.cpue2$Lake == "5 Island"] <- 393.759
std.cpue2$LakeSize[std.cpue2$Lake == "Blue"] <- 108.86
std.cpue2$LakeSize[std.cpue2$Lake == "Center"] <- 89.0308
std.cpue2$LakeSize[std.cpue2$Lake == "Silver"] <- 421.2778
std.cpue2$LakeSize[std.cpue2$Lake == "Storm"] <- 1253.311
std.cpue2$LakeSize[std.cpue2$Lake == "S Twin"] <- 224.601
std.cpue2$LakeSize[std.cpue2$Lake == "N Twin"] <- 183.323

# Max Depth (meters)
std.cpue2$MaxDepth[std.cpue2$Lake == "5 Island"] <- 7.58952
std.cpue2$MaxDepth[std.cpue2$Lake == "Blue"] <- 3.56616
std.cpue2$MaxDepth[std.cpue2$Lake == "Center"] <- 5.45592
std.cpue2$MaxDepth[std.cpue2$Lake == "Silver"] <- 3.23088
std.cpue2$MaxDepth[std.cpue2$Lake == "Storm"] <- 6.21792
std.cpue2$MaxDepth[std.cpue2$Lake == "S Twin"] <- 1.64592
std.cpue2$MaxDepth[std.cpue2$Lake == "N Twin"] <- 3.6576

# Mean Depth (meters)
std.cpue2$MeanDepth[std.cpue2$Lake == "5 Island"] <- 1.70688
std.cpue2$MeanDepth[std.cpue2$Lake == "Blue"] <- 1.6764
std.cpue2$MeanDepth[std.cpue2$Lake == "Center"] <- 3.71856
std.cpue2$MeanDepth[std.cpue2$Lake == "Silver"] <- 1.9812
std.cpue2$MeanDepth[std.cpue2$Lake == "Storm"] <- 2.62128
std.cpue2$MeanDepth[std.cpue2$Lake == "S Twin"] <- 1.09728
std.cpue2$MeanDepth[std.cpue2$Lake == "N Twin"] <- 2.52984

# Shoreline Development Index
std.cpue2$SDI[std.cpue2$Lake == "5 Island"] <- 3.6
std.cpue2$SDI[std.cpue2$Lake == "Blue"] <- 4.6
std.cpue2$SDI[std.cpue2$Lake == "Center"] <- 1.7
std.cpue2$SDI[std.cpue2$Lake == "Silver"] <- 1.8
std.cpue2$SDI[std.cpue2$Lake == "Storm"] <- 1.7
std.cpue2$SDI[std.cpue2$Lake == "S Twin"] <- 1.3
std.cpue2$SDI[std.cpue2$Lake == "N Twin"] <- 1.9

colnames(std.cpue2)[4]<-"WaterTemp"

head(std.cpue2)
# wow, that might be it!!
################################################################
std.cpue2$Year<-year(mdy(std.cpue2$Date))
# NOPE!
# Need logN and sdlogpop for each lake too
std.cpue2$logPop[std.cpue2$Lake == "5 Island" & 
                 std.cpue2$Species == "BIB" & 
                 std.cpue2$Year == 2018] <- log(2300)
std.cpue2$logPop[std.cpue2$Lake == "5 Island" & 
                 std.cpue2$Species == "COC" & 
                 std.cpue2$Year == 2018] <- log(25798)
std.cpue2$logPop[std.cpue2$Lake == "5 Island" & 
                   std.cpue2$Species == "BIB" & 
                   std.cpue2$Year == 2019] <- log(1303)
std.cpue2$logPop[std.cpue2$Lake == "5 Island" & 
                   std.cpue2$Species == "COC" & 
                   std.cpue2$Year == 2019] <- log(19738)

std.cpue2$logPop[std.cpue2$Lake == "Blue" &
                 std.cpue2$Species == "BIB" &
                 std.cpue2$Year == 2018] <- NA
std.cpue2$logPop[std.cpue2$Lake == "Blue" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- log(72140)
std.cpue2$logPop[std.cpue2$Lake == "Blue" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- NA
std.cpue2$logPop[std.cpue2$Lake == "Blue" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- log(25661)

std.cpue2$logPop[std.cpue2$Lake == "Center" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- log(4273)
std.cpue2$logPop[std.cpue2$Lake == "Center" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- log(6466)
std.cpue2$logPop[std.cpue2$Lake == "Center" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- log(3840)
std.cpue2$logPop[std.cpue2$Lake == "Center" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- log(1451)

std.cpue2$logPop[std.cpue2$Lake == "Silver" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- log(12486)
std.cpue2$logPop[std.cpue2$Lake == "Silver" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- log(9755)
std.cpue2$logPop[std.cpue2$Lake == "Silver" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- log(3767)
std.cpue2$logPop[std.cpue2$Lake == "Silver" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- log(9174)

std.cpue2$logPop[std.cpue2$Lake == "Storm" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- log(13724)
std.cpue2$logPop[std.cpue2$Lake == "Storm" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- log(9251)
std.cpue2$logPop[std.cpue2$Lake == "Storm" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- log(158)
std.cpue2$logPop[std.cpue2$Lake == "Storm" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- log(15467)

std.cpue2$logPop[std.cpue2$Lake == "S Twin" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- log(34277)
std.cpue2$logPop[std.cpue2$Lake == "S Twin" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- log(20661)
std.cpue2$logPop[std.cpue2$Lake == "S Twin" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- log(11648)
std.cpue2$logPop[std.cpue2$Lake == "S Twin" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- log(14896)

std.cpue2$logPop[std.cpue2$Lake == "N Twin" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- log(18776)
std.cpue2$logPop[std.cpue2$Lake == "N Twin" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- log(3816)
std.cpue2$logPop[std.cpue2$Lake == "N Twin" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- log(20423)
std.cpue2$logPop[std.cpue2$Lake == "N Twin" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- log(2487)

#####################################################################################

# Now the SD Log Pop
std.cpue2$sdlogPop[std.cpue2$Lake == "5 Island" & 
                   std.cpue2$Species == "BIB" & 
                   std.cpue2$Year == 2018] <- 0.5326105998
std.cpue2$sdlogPop[std.cpue2$Lake == "5 Island" & 
                   std.cpue2$Species == "COC" & 
                   std.cpue2$Year == 2018] <- 0.1226530201
std.cpue2$sdlogPop[std.cpue2$Lake == "5 Island" & 
                   std.cpue2$Species == "BIB" & 
                   std.cpue2$Year == 2019] <- 1.517427129
std.cpue2$sdlogPop[std.cpue2$Lake == "5 Island" & 
                   std.cpue2$Species == "COC" & 
                   std.cpue2$Year == 2019] <- 0.3257476759

std.cpue2$sdlogPop[std.cpue2$Lake == "Blue" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- NA
std.cpue2$sdlogPop[std.cpue2$Lake == "Blue" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- 0.00104523218
std.cpue2$sdlogPop[std.cpue2$Lake == "Blue" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- NA
std.cpue2$sdlogPop[std.cpue2$Lake == "Blue" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- 0.2329193312

std.cpue2$sdlogPop[std.cpue2$Lake == "Center" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- 0.2958754184
std.cpue2$sdlogPop[std.cpue2$Lake == "Center" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- 0.2729264589
std.cpue2$sdlogPop[std.cpue2$Lake == "Center" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- 1.00578373
std.cpue2$sdlogPop[std.cpue2$Lake == "Center" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- 0.3668237592

std.cpue2$sdlogPop[std.cpue2$Lake == "Silver" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- 0.7585276164
std.cpue2$sdlogPop[std.cpue2$Lake == "Silver" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- 0.1612536189
std.cpue2$sdlogPop[std.cpue2$Lake == "Silver" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- 0.2545944848
std.cpue2$sdlogPop[std.cpue2$Lake == "Silver" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- 1.517427129

std.cpue2$sdlogPop[std.cpue2$Lake == "Storm" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- 1.517427129
std.cpue2$sdlogPop[std.cpue2$Lake == "Storm" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- 0.1245415915
std.cpue2$sdlogPop[std.cpue2$Lake == "Storm" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- 1.517427129
std.cpue2$sdlogPop[std.cpue2$Lake == "Storm" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- 0.2467117035

std.cpue2$sdlogPop[std.cpue2$Lake == "S Twin" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- 2.844501878
std.cpue2$sdlogPop[std.cpue2$Lake == "S Twin" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- 0.1874903214
std.cpue2$sdlogPop[std.cpue2$Lake == "S Twin" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- 0.7585276164
std.cpue2$sdlogPop[std.cpue2$Lake == "S Twin" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- 0.2837096018

std.cpue2$sdlogPop[std.cpue2$Lake == "N Twin" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2018] <- 0.00384848064
std.cpue2$sdlogPop[std.cpue2$Lake == "N Twin" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2018] <- 0.00810169953
std.cpue2$sdlogPop[std.cpue2$Lake == "N Twin" &
                   std.cpue2$Species == "BIB" &
                   std.cpue2$Year == 2019] <- 0.1436441939
std.cpue2$sdlogPop[std.cpue2$Lake == "N Twin" &
                   std.cpue2$Species == "COC" &
                   std.cpue2$Year == 2019] <- 0.1982492871


head(std.cpue2)
std.cpue2<-droplevels(subset(std.cpue2, !is.na(std.cpue2$logPop)))

summary(std.cpue2$LakeSize)



########################################################################
#
#
#         JAGS Models
#
#
########################################################################
# define model
library(rjags)
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
## draw curve of estimated catchabaility as function of air temp and wind using betas
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
  ggtitle("Carp and Buffalo Lakes: Intercept")
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
  ggtitle("Carp and Buffalo Lakes: Lake Size")
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
  ggtitle("Carp and Buffalo Lakes: Maximum Depth")
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
  ggtitle("Carp and Buffalo Lakes: Mean Depth")
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
  ggtitle("Carp and Buffalo Lakes: SDI")
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
  ggtitle("Carp and Buffalo Lakes: Water Temp")
beta5












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