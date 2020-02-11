# 2019 age structure inventory for google sheets

rm(list=ls())


packages<-function(x, repos="http://cran.r-project.org", ...){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x, repos=repos, ...)
    require(x,character.only=TRUE)
  }
}


packages(rcompanion)
packages(vegan)
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
packages(GISTools)
packages(maps)
packages(mapproj)



df<-read.csv("2017-2020 Compiled Data.csv",header = T)
str(df)
df$Year<-as.factor(df$Year)
df$Date<-as.Date(df$Date, "%m/%d/%Y")
df$On.Time..s.<-as.numeric(df$On.Time..s.)
str(df)

# subset for 2019
df2019<-droplevels(subset(df, Year == "2019"))

# subset for carp and buff
df2019<-droplevels(subset(df2019, Species == "COC" |
                                  Species == "BIB"))

# look for aging fish in notes, and all fish weighed??
levels(df2019$Notes)
# subset here: first is non-NA weights, then all aging fish / spine notes too
ages2019<-droplevels(subset(df2019, !is.na(df2019$Weight..g.) |
                                    Notes == "2018 tag loss, spine" |                                     
                                    Notes == "2019 recap, spine taken"|                                
                                    Notes == "2019 recap/spine cut"  |                                   
                                    Notes == "Aging fish"             |                                  
                                    Notes == "Aging Fish"              |                                 
                                    Notes == "Aging Fish "              |                                
                                    Notes == "Aging fish - but no weight"|                               
                                    Notes == "Bloody, Spine"              |                              
                                    Notes == "Cracked O, spine"            |                             
                                    Notes == "spine"                        |                            
                                    Notes == "Spine"                         |                           
                                    Notes == "Spine "                         |                          
                                    Notes == "Spine 2018 tag loss"             |                         
                                    Notes == "Spine sampling loss, R pelvic clip"|                       
                                    Notes == "Spine Sickly"     |                                        
                                    Notes == "Spine taken"       |                                       
                                    Notes == "Spine Taken"        |                                      
                                    Notes == "Spine Taken, No Weight"|                                   
                                    Notes == "Spine, bent tag 27987"))
summary(ages2019$Species)

str(ages2019)
# remove weights without envelopes (fyke and inital s twin day when we removed buff for hort farm)
# this will also remove any weights where a spine might have been cut... and mounted. 
# # # # We will skip those if not in file
ages2019<-droplevels(subset(ages2019, TAG.NUMBER != ""))
ages2019<-ages2019[,-c(7,8,11:22)]


# good enough for government work :)
write.csv(ages2019, "2019 Age Structure Inventory.csv",row.names=F)
