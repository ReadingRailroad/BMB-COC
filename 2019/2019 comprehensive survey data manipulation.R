# Meerbeek data request, part 2

# 2019 mean TL, in both mm and inches, for all species captured in Center, Five Island, Silver, and Storm



# read in .csv, subset to 2019 and only those four lakes
setwd("~/BMB-COC")
df<-read.csv("2017-2020 Compiled Data.csv", header = T)
df$Date<-as.Date(df$Date, "%m/%d/%Y")
df$On.Time..s.<-as.numeric(df$On.Time..s.)
df$Year<-as.factor(df$Year)
str(df)
#
df2<-droplevels(subset(df, Year == "2019"))
df2<-droplevels(subset(df2, Lake == "Center" |
                            Lake == "5 Island" |
                            Lake == "Silver"  |
                            Lake == "Storm"))
df2<-subset(df2, !is.na(df2$Length..mm.))

# split e-fishing and fyke
df.fyke<-droplevels(subset(df2, Gear == "Fyke"))
df.efish<-droplevels(subset(df2, Gear == "Electroshocking"))

# Fyke Mean total length, melt and cast by species!
library(reshape2)
df.fyke.m<-melt(df.fyke, id.vars = c("Lake","Site.Transect","Species"), 
                measure.vars = "Length..mm.")
fyke.tl.cast<-dcast(df.fyke.m, Species ~ Lake, value.var = "value", mean)
fyke.tl.cast[,c(2:4)]<-round(fyke.tl.cast[,c(2:4)]/25.4,1)


# Electrofishing mean total length, melted and casted by species
df.efish.m<-melt(df.efish, id.vars = c("Lake","Site.Transect","Species"), 
                 measure.vars = "Length..mm.")
efish.tl.cast<-dcast(df.efish.m, Species ~ Lake, value.var = "value",mean)
efish.tl.cast[,c(2:5)]<-round(efish.tl.cast[,c(2:5)]/25.4,1)
########################################################################################################


# Now the CPUE spreadsheet
## Fyke Net CPUE is N/Net
df.fyke.m<-melt(df.fyke, id.vars = c("Lake","Site.Transect","Species"), 
                measure.vars = "detect")
fyke.cpue.cast<-dcast(df.fyke.m, Species ~ Lake, value.var = "value")
# need to divide by number of nets in each lake to get average # fish per net for each species in each lake
fyke.cpue.cast[,2]<-round(fyke.cpue.cast[,2]/6,1) # 6 nets in center lake
fyke.cpue.cast[,3]<-round(fyke.cpue.cast[,3]/11,1) # 11 nets in Silver Lake
fyke.cpue.cast[,4]<-round(fyke.cpue.cast[,4]/10,1) # 10 nets in Storm Lake


## Electrofishing CPUE is N/hour (SPRING EF STD RUNS ONLY!! and no bonus runs)
df.efish<-droplevels(subset(df.efish, Date == "2019-06-03" & Site.Transect != "Bonus 2" & Site.Transect != "Bonus 3" | # silver and center
                           Date == "2019-05-15" & Site.Transect != "Bonus" | # storm
                           Date == "2019-06-17" & Site.Transect != "Bonus 1")) # five island
levels(df.efish$Site.Transect)
df.efish$Effort<-df.efish$On.Time..s./3600
efish.cpue<-aggregate(detect~Lake+Species + Site.Transect + Effort, data = df.efish, sum)
efish.cpue$cpue<-efish.cpue$detect/efish.cpue$Effort
cpue<-aggregate(cpue ~ Lake + Species, data = efish.cpue, mean)

df.efish.m<-melt(cpue)
efish.cpue.cast<-dcast(df.efish.m, Species ~ Lake, value.var = "value")
efish.cpue.cast[,c(2:5)]<-round(efish.cpue.cast[,c(2:5)],1)
