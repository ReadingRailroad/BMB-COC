# meerbeek data request part 3: Meerbeek cubed
# data request sent by JM 4-8-2020

rm(list=ls())

packages<-function(x, repos="http://cran.r-project.org", ...){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x, repos=repos, ...)
    require(x,character.only=TRUE)
  }
}

packages(ggplot2)
packages(gridExtra)
packages(reshape2)
packages(FSA)
packages(dplyr)
packages(tidyr)
packages(rcompanion)
packages(ggpmisc)



df<-read.csv("2017-2020 Compiled Data.csv")
str(df)

df$Year<-as.factor(df$Year)
df$TAG.NUMBER<-as.factor(df$TAG.NUMBER)
df$Date<-as.Date(df$Date, "%m/%d/%Y")
df$On.Time..s.<-as.numeric(df$On.Time..s.)
str(df)

# Fish Age Structures in 2017 
df2<-read.csv("2017_Age_Estimates.csv",header=T)
# spines
length(df2$Species[df2$Species == "COC"])
# 436 Carp spines

length(df2$Species[df2$Species == "BIB"])
# 324 Buffalo spines

#otoliths
length(df2$Species[df2$Species == "COC" &
                   df2$Lake != "Center"])
# 308 Carp Otoliths

length(df2$Species[df2$Species == "BIB" &
                     df2$Lake != "Center"])
# 204 Buff otoliths





###########################################3
# 2018 carp and buff ages:
df2<-read.csv("2018 Age Structure Estimates.csv")
# spines
length(df2$Species[df2$Species == "COC"])
# 837 Carp spines

length(df2$Species[df2$Species == "BIB"])
# 557 Buffalo spines


levels(df2$Lake)


#otoliths
length(df2$Species[df2$Species == "COC" & df2$Lake == "Center" |
                     df2$Species == "COC" & df2$Lake == "Five Island" |
                     df2$Species == "COC" & df2$Lake == "Storm" |
                     df2$Species == "COC" & df2$Lake == "Silver"])
# 470 Carp Otoliths

length(df2$Species[df2$Species == "BIB" & df2$Lake == "Center" |
                     df2$Species == "BIB" & df2$Lake == "Five Island" |
                     df2$Species == "BIB" & df2$Lake == "Storm" |
                     df2$Species == "BIB" & df2$Lake == "Silver"])
# 371 Buff otoliths





# total number of age structures for each species during comprehensive surveys for 2019 by lake:
df$TAG.NUMBER[df$TAG.NUMBER == ""] <- NA
df<-droplevels(subset(df, Year == 2019 &
                          Gear == "Fyke" &
                          !is.na(df$TAG.NUMBER)))

df.c<-dcast(df, Lake ~ Species)


