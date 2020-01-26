# meerbeek data request January 23, 2020
library(rcompanion)
df<-read.csv("2017-2020 Compiled Data.csv")
str(df)

# only carp
df<-droplevels(subset(df, Species == "COC" |
                          Species == "dummy"))

# only electrofishing
levels(df$Gear)
df<-droplevels(subset(df, Gear == "Electroshocking"))

# August - October only
df$Date<-as.Date(df$Date, "%Y-%m-%d")
str(df$Date)
df$Month<-factor(format(df$Date, "%m"))
df2<-droplevels(subset(df, Month == "08" |
                          Month == "09" |
                          Month == "10"))

# standard runs only:
levels(df2$Site.Transect)
df2<-droplevels(subset(df2, Site.Transect == "Std 1" |
                          Site.Transect == "Std 2" |
                          Site.Transect == "Std 3" |
                          Site.Transect == "Std 4" |
                          Site.Transect == "Std 5" |
                          Site.Transect == "Std 6" |
                          Site.Transect == "Std 7" |
                          Site.Transect == "Std 9"))


# average weight by lake and year
str(df2)
groupwiseMean(Weight..g.~Lake + Year,
              data = df2)

# checking to see the most weights we can get:

df3<-droplevels(subset(df, Month == "06" |
                           Month == "07" |
                           Month == "08" |
                           Month == "09" |
                           Month == "10"))
groupwiseMean(Weight..g.~Lake + Year, data = df3)
###################################
# Jonathan meerbeek says using the L-W regression from spring weights on lengths of fall fish is OK
###################################

# l-w regression from all coc data in each year

# apply slope and intercept model to observed fall lengths

# subset for fall lengths (std runs aug-oct only)

# create expected weight column, average by lake and year.








# average CPUE by lake and year
# using df2 from above

# need to aggregate data by lake and year, then also average catch per hour for all transects in that aggregation?
library(reshape2)
str(df2$On.Time..s.)
df2$On.Time..s.<-as.numeric(df2$On.Time..s.)
catch.df<-dcast(df2, Lake+Year+Site.Transect+On.Time..s.+Species~ detect)

# good so far.
# Need to add 0 catches from 2018 ('dummy' species represents 0 catches in 2019)
## none for blue lake or five island. Other five lakes are here:

str(catch.df)
#center lake
a<-c("Center","2018","Std 1",1003,"COC",0)
b<-c("Center","2018","Std 4",870,"COC",0)
c<-c("Center","2018","Std 2",885,"COC",0)
d<-c("Center","2018","Std 4",778,"COC",0)
e<-c("Center","2018","Std 2",849,"COC",0)
f<-c("Center","2018","Std 4",549,"COC",0)

# North Twin
g<-c("N Twin","2018","Std 5",900,"COC",0)
h<-c("N Twin","2018","Std 3",900,"COC",0)
i<-c("N Twin","2018","Std 1",900,"COC",0)
j<-c("N Twin","2018","Std 2",900,"COC",0)
k<-c("N Twin","2018","Std 5",900,"COC",0)

# silver
l<-c("Silver","2018","Std 1",1116,"COC",0)
m<-c("Silver","2018","Std 5",549,"COC",0)
n<-c("Silver","2018","Std 6",804,"COC",0)

# south twin
o<-c("S Twin","2018","Std 1",1471,"COC",0)
p<-c("S Twin","2018","Std 4",1200,"COC",0)
q<-c("S Twin","2018","Std 4",900,"COC",0)

# storm
r<-c("Storm","2018","Std 5", 954,"COC",0)
s<-c("Storm","2018","Std 7", 445,"COC",0)
t<-c("Storm","2018","Std 1", 902,"COC",0)
u<-c("Storm","2018","Std 5", 804,"COC",0)
v<-c("Storm","2018","Std 5", 1623,"COC",0)
w<-c("Storm","2018","Std 9", 804,"COC",0)

add.on<-data.frame(t(data.frame(a,b,c,d,e,f,g,h,i,g,k,l,m,n,o,p,q,r,s,t,u,v,w)),
                   stringsAsFactors = F)
names(add.on)<-c("Lake","Year","Site.Transect","On.Time..s.","Species","1")
str(add.on)
add.on$`1`<-as.numeric(add.on$`1`)
add.on$On.Time..s.<-as.numeric(add.on$On.Time..s.)
add.on$Site.Transect<-as.factor(add.on$Site.Transect)
add.on$Lake<-as.factor(add.on$Lake)
add.on$Year<-as.factor(add.on$Year)
add.on$Species<-as.factor(add.on$Species)

str(add.on)

cpue.df<-rbind(catch.df,add.on)

cpue.df$CPUE<-cpue.df$`1`/(cpue.df$On.Time..s./3600)

groupwiseMean(CPUE~ Lake+Year,
              cpue.df)
# looking good, son!