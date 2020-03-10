# developing comprehensive age structure inventory - sportfish 
library(reshape2)
library(ggplot2)
library(gridExtra)
library(wesanderson)

library(tidyverse)

# Otoliths collected from dissections in 2018 and 2019

df<-read.csv("2017-2020 Compiled Data.csv", header = T)
df$Species[df$Species == "YLP"]<-"YEP"
df<-droplevels(subset(df, Species != "COC" &
                          Species != "BIB" &
                          Species != "GZS")) # drop gizzard shad just for fun
levels(df$TAG.NUMBER)
df$TAG.NUMBER[df$TAG.NUMBER == ""] <- NA
df.live<-droplevels(subset(df, is.na(df$TAG.NUMBER))) # for later
df<-droplevels(subset(df, !is.na(df$TAG.NUMBER)))
df$Notes<-"Dissected"

levels(df$Lake)

# control shift M


# need to add in N Twin and S Twin!!
newdf<-read.csv("2018/Twin Lake 2018 Fish Dissections.csv", header = T)
str(newdf)
levels(newdf$Species)
newdf$Species[newdf$Species == "BLG "]<-"BLG"
newdf$Species[newdf$Species == "WHC "]<-"WHC"
summary(newdf$Year)
newdf <- unite(newdf,"TAG.number",Lake,TAG.NUMBER,sep="-",remove = F) 
newdf<-newdf[,c(1,3,2,5:23)]
colnames(newdf)<-colnames(df)
newdf$Notes<-"Dissected"
head(newdf)

df<-droplevels(rbind(df,newdf))
levels(df$Species)


# merge back in with Alive fish from line 18
df.live<-droplevels(subset(df.live, Species == "BLC" |
                                    Species == "BLG" |
                                    Species == "GSF" |
                                    Species == "LMB" |
                                    Species == "Pumpkinseed" |
                                    Species == "WHC" |
                                    Species == "YEP" |
                                    Species == "WAE"))
df.live$Notes<-"Released"

# knock it back to just 2018


df2<-droplevels(rbind(df,df.live))
levels(df2$Notes)
# knock it back to just 2018
df2<-droplevels(subset(df2, Year == 2018))
junk<-melt(df2, id.var = c("Species","Lake","Notes"),
           measure.vars = "detect")
LakeTotals<-dcast(junk,Lake+Notes~Species)
LakeTotals
