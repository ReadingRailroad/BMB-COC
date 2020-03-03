# developing comprehensive age structure inventory - sportfish 

# Otoliths collected from dissections in 2018 and 2019

df<-read.csv("2017-2020 Compiled Data.csv", header = T)

df<-droplevels(subset(df, Species != "COC" &
                          Species != "BIB" &
                          Species != "GZS"))
# need to check and see about sportfish otoliths collected from N and S Twin
# Euthanized in notes doesn't work for those two lakes. 
levels(df$Lake)
df2<-droplevels(subset(df, Lake == "N Twin" |
                           Lake == "S Twin"))
levels(df2$TAG.NUMBER) # bad news bears

# we have weight observations, I assume those are the dissections.
## Need to go back in time and change my entire life
### Specifically, go back and find the envelope numbers from N and S Twin 2018 Otoliths.








levels(df$TAG.NUMBER)
df$TAG.NUMBER[df$TAG.NUMBER == ""] <- NA

df<-droplevels(subset(df, !is.na(df$TAG.NUMBER)))

library(reshape2)

df.c<-dcast(df, Species + Year ~ Lake + detect)

# need to check and see about sportfish otoliths collected from N and S Twin

