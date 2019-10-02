# Front Matter

rm(list=ls())


packages<-function(x, repos="http://cran.r-project.org", ...){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x, repos=repos, ...)
    require(x,character.only=TRUE)
  }
}

#remove.packages(c("ggplot2", "data.table"))
#install.packages('Rcpp', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('data.table', dependencies = TRUE)

#packages(rcompanion)
#packages(vegan)
packages(ggplot2)
packages(gridExtra)
packages(reshape2)
packages(FSA)
packages(nnet)
#packages(multcomp)
#packages(plyr)
#packages(rv)
packages(arm)
packages(lattice)
packages(tikzDevice)
packages(FSAdata)
packages(nlstools)
#packages(GGally)
packages(wesanderson)
#packages(DescTools)
#packages(mvnormtest)
#packages(HH)
#packages(biotools)
#packages(car)
#packages(psych)
#packages(GPArotation)
packages(doBy)
packages(dplyr)
packages(tidyr)
packages(rcompanion)
packages(GISTools)
packages(maps)
packages(mapproj)






# age-frequency histograms

ages<-read.csv("2017_Age_Estimates.csv",header=T)
ages2<-read.csv("2018 Age Estimates.csv",header=T)

# 2017 data
ages<-droplevels(ages[-761,])
summary(ages)
str(ages)
ages$Year<-as.factor(2017)

# 2018 data
str(ages2)
ages2$Year<-as.factor(ages2$X)
summary(ages2)

age<-ages[,c(14,1,3,6)]
age0<-ages2[,c(16,2,4,10)]
head(age)
colnames(age0)<-c("Year","Lake","Species","Spine.MAS")

age0$Lake<-as.character(age0$Lake)
age0$Lake[age0$Lake == "N. Twin"]<-"N Twin"
age0$Lake<-factor(age0$Lake)


ages<-rbind(age,age0)

ages<-droplevels(subset(ages, !is.na(ages$Spine.MAS)))

ages$Species<-as.character(ages$Species)
ages$Species[ages$Species == "BIB"] <-"Bigmouth Buffalo"
ages$Species[ages$Species == "COC"] <-"Common Carp"
ages$Species<-as.factor(ages$Species)

summary(ages$Spine.MAS)

ages.c<-droplevels(subset(ages, Species == "Common Carp"))

Age.Hist.C<-ggplot(ages.c, aes(x=Spine.MAS))+
  geom_histogram(binwidth = 1,
                 colour = "white")+
  facet_grid(Year~Lake)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey20"))+
  #scale_fill_grey(start = 0.4, end = 0.9, aesthetics = "fill")+
  labs(title = "Common Carp Age Frequency",
       x = "Age",
       y = "Frequency")

Age.Hist.C

ages.b<-droplevels(subset(ages, Species == "Bigmouth Buffalo"))

Age.Hist.B<-ggplot(ages.b, aes(x=Spine.MAS))+
  geom_histogram(binwidth = 1,
                 colour = "white")+
  facet_grid(Year~Lake)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey20"))+
  #scale_fill_grey(start = 0.4, end = 0.9, aesthetics = "fill")+
  labs(title = "Bigmouth Buffalo Age Frequency",
       x = "Age",
       y = "Frequency")

Age.Hist.B

