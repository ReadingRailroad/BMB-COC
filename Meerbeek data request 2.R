# meerbeek data request part 2: Electric Boogaloo
# Problems/errors from email sent by JM 2-19-2020

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

# subset to just 2019
df2<-droplevels(subset(df, Year == "2019"))

#### addressing the lake/spp/gear issues one at a time

# Five Island yellow bullhead. shows 5 fish/hr but 0 mean total length
df3<-droplevels(subset(df2, Lake == "5 Island"))
levels(df3$Species) # no YLB (yellow bullhead in step)
df4<-droplevels(subset(df3, Species == "YEP")) 
(mean(df4$Length..mm.,na.rm=T))/25.4 # length for five island perch maches, but no CPUE in Jon's table
# find cpue for five island yellow perch here
## getting rid of junk first
df4<-df4[,c(4,12,17,22)]
df4$Effort<-df4$On.Time..s./3600

df5<-dcast(df4, Site.Transect~detect)
df4$Effort
df5$Effort<-c(0.2797222,0.3152778,0.4147222)
df5$CPUE<-df5$`1`/df5$Effort
# final average cpue here, added 2 to sample size to reflect the two transects without YEP
sum(df5$CPUE/5)

#silver lake white sucker (4 cpue, but no TL)
#Silver lake yellow bullhead (3.7 cpue, but no TL)
# silver lake yellow perch (0 cpue, but 10.5 mean TL)
df3<-droplevels(subset(df2, Lake == "Silver" &
                            Gear == "Electroshocking"))
levels(df3$Species) # no YLB (yellow bullhead) or WHS (white sucker)
df3$Length..mm.[df3$Species == "YEP"]/25.4
df4<-df3[,c(4,12,17,22)]
df4$Effort<-df4$On.Time..s./3600

df5<-aggregate(detect ~ Species + Site.Transect + Effort, data = df4, sum)
df5$CPUE<-df5$detect/df5$Effort

df5.yep<-droplevels(subset(df5, Species == "YEP"))
df5.yep$CPUE/6

# storm black crappie
df3<-droplevels(subset(df2, Lake == "Storm" &
                            Gear == "Electroshocking"))
levels(df3$Species) # fyke not e-fishing

#########################################################################

# center lake common carp in fyke nets
df3<-droplevels(subset(df2, Lake == "Center" & Gear == "Fyke"))
levels(df3$Species)

# storm lake yellow bullhead
df3<-droplevels(subset(df2, Lake == "Storm" & Gear == "Fyke"))
levels(df3$Species) # YEB exist (Yellow bullhead in this case)
df4<-droplevels(subset(df3, Species == "YEB"))
(mean(df4$Length..mm.))/25.4 # 7.9 average inches for yellow bullhead
