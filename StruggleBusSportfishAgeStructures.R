# developing comprehensive age structure inventory - sportfish 

# Otoliths collected from dissections in 2018 and 2019

df<-read.csv("2017-2020 Compiled Data.csv", header = T)

df<-droplevels(subset(df, Species != "COC" &
                          Species != "BIB" &
                          Species != "GZS"))
df$Species[df$Species == "YLP"]<-"YEP"
junk<-melt(df, id.var = c("Species","Lake","Year"),
           measure.vars = "detect")
LakeTotals<-dcast(junk,Lake+Year~Species)

LakeTotals<-LakeTotals[,c(1,2,5,6,7,15,18,27,29,33)]
LakeTotals

# need to check and see about sportfish otoliths collected from N and S Twin
# Euthanized in notes doesn't work for those two lakes. 
datframe<-read.csv("2018_COC_BIB_CMR_Data.csv", header = T)
str(datframe)
# Lakes
levels(datframe$Lake)
datframe$detect<-1
# this is good for overall total catch, HOWEVER:
#   I need to split by released alive and euthanized

levels(datframe$Notes)
datframe<-droplevels(subset(datframe, Species != "BIB" &
                                      Species != "COC"))
datframe<-droplevels(subset(datframe, Lake == "N. Twin" |
                                      Lake == "S. Twin"))
length(datframe$Species[datframe$Notes == "euthanized"])
datframe$Notes[datframe$Notes == "Aging Fish" |
                 datframe$Notes == "Aging Fish " |
                 datframe$Notes == "Aging Fish - 2017 Tag Loss" |
                 datframe$Notes == "Aging Fish - 2017 Tag Loss Female" |
                 datframe$Notes == "Aging Fish - Female" |
                 datframe$Notes == "Aging Fish - Male" |
                 datframe$Notes == "Aging Fish - Mirror" |
                 datframe$Notes == "Aging Fish - Scoliosis" |
                 datframe$Notes == "Aging Fish 2017 Tag Loss Female"] <- "euthanized"
datframe$Notes<-droplevels(datframe$Notes)
levels(datframe$Notes)
df<-droplevels(subset(datframe, Notes == "euthanized"))




# we have weight observations, I assume those are the dissections.
## Need to go back in time and change my entire life
### Specifically, go back and find the envelope numbers from N and S Twin 2018 Otoliths.








levels(df$TAG.NUMBER)
df$TAG.NUMBER[df$TAG.NUMBER == ""] <- NA

df<-droplevels(subset(df, !is.na(df$TAG.NUMBER)))

library(reshape2)

df.c<-dcast(df, Species + Year ~ Lake + detect)

# need to check and see about sportfish otoliths collected from N and S Twin

