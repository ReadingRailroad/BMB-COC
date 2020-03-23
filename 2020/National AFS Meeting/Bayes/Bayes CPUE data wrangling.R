# Bayes Data code

setwd("~/BMB-COC/2020/National AFS Meeting/Bayes/") # set file directory


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

# write.csv(std.cpue2, "CPUE Bayes Data.csv",row.names=F)