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
                                      Lake == "S. Twin" ))
levels(datframe$Notes)
df.check<-datframe

df.check$Length..mm.<-df.check$Length..mm./25.4 # convert mm to inches

summary(df.check$Weight..g.)
df.check$Weight..g.<-df.check$Weight..g./28.3495 

summary(df.check)
write.csv(df.check, "2018/2018 Twin Lake Sportfish Age Envelope.csv",row.names = F)



# we have weight observations, I assume those are the dissections.
## Need to go back in time and change my entire life
### Specifically, go back and find the envelope numbers from N and S Twin 2018 Otoliths.








levels(df$TAG.NUMBER)
df$TAG.NUMBER[df$TAG.NUMBER == ""] <- NA

df<-droplevels(subset(df, !is.na(df$TAG.NUMBER)))

library(reshape2)

df.c<-dcast(df, Species + Year ~ Lake + detect)

# need to check and see about sportfish otoliths collected from N and S Twin

