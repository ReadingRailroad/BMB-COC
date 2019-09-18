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
ages2$Year<-as.factor(ages2$Year)
summary(ages2)

age<-ages[,c(14,1,3,6)]
age0<-ages2[,c(1,2,4,10)]
head(age)
colnames(age0)<-c("Year","Lake","Species","Spine.MAS")

ages<-rbind(age,age0)

ages<-droplevels(subset(ages, !is.na(ages$Spine.MAS)))

ages$Species<-as.character(ages$Species)
ages$Species[ages$Species == "BIB"] <-"Bigmouth Buffalo"
ages$Species[ages$Species == "COC"] <-"Common Carp"
ages$Species<-as.factor(ages$Species)

summary(ages$Spine.MAS)

Age.Hist<-ggplot(ages, aes(x=Spine.MAS, fill = Year))+
  geom_histogram(binwidth = 1,
                 colour = "white")+
  facet_grid(Species~Lake)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey20"))+
  #scale_fill_grey(start = 0.4, end = 0.9, aesthetics = "fill")+
  labs(title = NULL,
       x = "Age",
       y = "Frequency")

Age.Hist

df.c.m<-melt(df.c)

all.lakes.Carp.lf<-ggplot(df.c.m, aes(x=value, fill = Gear))+
  geom_histogram(binwidth = 1,
                 colour = "black",
                 position = "stack")+
  coord_cartesian(xlim = c(0,45))+
  facet_grid(vars(Year), vars(Lake))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey20"))+
  scale_fill_grey(start = 0.1, end = 0.9, aesthetics = "fill")+
  labs(title = "2017 and 2018 Common Carp Length Frequencies",
       x = "Length (inches)",
       y = "Frequency")

all.lakes.Carp.lf

grid.arrange(all.lakes.Buff.lf,all.lakes.Carp.lf,ncol=1)



