# meerbeek data request January 23, 2020

rm(list=ls())

df<-read.csv("2017-2020 Compiled Data.csv")
str(df)

# only carp
df<-droplevels(subset(df, Species == "COC" |
                          Species == "dummy"))


# Only electrofishing
levels(df$Gear)
df2<-droplevels(subset(df, Gear == "Electroshocking"))

# August - October only
df2$Date<-as.Date(df2$Date, "%m/%d/%Y")
str(df2$Date)
df2$Month<-factor(format(df2$Date, "%m"))
df2<-droplevels(subset(df2, Month == "08" |
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


###################################
# Jonathan meerbeek says using the L-W regression from spring weights on lengths of fall fish is OK
###################################

# l-w regression from all coc data in each year
#
#    Blue Lake
#
#
blue.df<-droplevels(subset(df, Lake == "Blue"))
blue.df<-subset(blue.df, !is.na(Weight..g.))
summary(blue.df$Weight..g.) # checking range
summary(blue.df$Length..mm.) # checking range
blue.df$Year<-as.factor(blue.df$Year)
#2017
blue.df.17<-droplevels(subset(blue.df, Year == "2017"))
blue.m.17<-lm(Weight..g. ~ Length..mm., data = blue.df.17)
summary(blue.m.17)
#2018
blue.df.18<-droplevels(subset(blue.df, Year == "2018"))
blue.m.18<-lm(Weight..g. ~ Length..mm., data = blue.df.18)
summary(blue.m.18)
#2019
blue.df.19<-droplevels(subset(blue.df, Year == "2019"))
blue.m.19<-lm(Weight..g. ~ Length..mm., data = blue.df.19)
summary(blue.m.19)

# apply slope and intercept model to observed fall lengths
#
# subset for fall lengths (std runs aug-oct only)
# Use df2 above, split for Blue and by years
blue.df2<-droplevels(subset(df2, Lake == "Blue"))
blue.df2.17<-droplevels(subset(blue.df2, Year == "2017"))
blue.df2.18<-droplevels(subset(blue.df2, Year == "2018"))
blue.df2.19<-droplevels(subset(blue.df2, Year == "2019"))

# create expected weight column, 
est.17<-predict(blue.m.17,blue.df2.17)
est.18<-predict(blue.m.18,blue.df2.18)
est.19<-predict(blue.m.19,blue.df2.19)

# average every year.
average.est.17<-mean(est.17,na.rm=T)
average.est.18<-mean(est.18,na.rm=T)
average.est.19<-mean(est.19,na.rm=T)

# l-w regression from all coc data in each year
#
#    Center Lake
#
#
Center.df<-droplevels(subset(df, Lake == "Center"))
Center.df<-subset(Center.df, !is.na(Weight..g.))
summary(Center.df$Weight..g.) # checking range
summary(Center.df$Length..mm.) # checking range
Center.df$Year<-as.factor(Center.df$Year)
#2017
Center.df.17<-droplevels(subset(Center.df, Year == "2017"))
Center.m.17<-lm(Weight..g. ~ Length..mm., data = Center.df.17)
summary(Center.m.17)
#2018
Center.df.18<-droplevels(subset(Center.df, Year == "2018"))
Center.m.18<-lm(Weight..g. ~ Length..mm., data = Center.df.18)
summary(Center.m.18)
#2019
Center.df.19<-droplevels(subset(Center.df, Year == "2019"))
Center.m.19<-lm(Weight..g. ~ Length..mm., data = Center.df.19)
summary(Center.m.19)

# apply slope and intercept model to observed fall lengths
#
# subset for fall lengths (std runs aug-oct only)
# Use df2 above, split for Center and by years
Center.df2<-droplevels(subset(df2, Lake == "Center"))
Center.df2.17<-droplevels(subset(Center.df2, Year == "2017"))
Center.df2.18<-droplevels(subset(Center.df2, Year == "2018"))
Center.df2.19<-droplevels(subset(Center.df2, Year == "2019"))

# create expected weight column, 
est.17<-predict(Center.m.17,Center.df2.17)
est.18<-predict(Center.m.18,Center.df2.18)
est.19<-predict(Center.m.19,Center.df2.19)

# average every year.
average.est.17<-mean(est.17,na.rm=T)
average.est.18<-mean(est.18,na.rm=T)
average.est.19<-mean(est.19,na.rm=T)

# l-w regression from all coc data in each year
#
#    Five Island Lake
#
#
Five.Island.df<-droplevels(subset(df, Lake == "5 Island"))
summary(Five.Island.df$Weight..g.[Five.Island.df$Year == "2018"])
Five.Island.df<-subset(Five.Island.df, !is.na(Weight..g.))
summary(Five.Island.df$Weight..g.) # checking range
summary(Five.Island.df$Length..mm.) # checking range
Five.Island.df$Year<-as.factor(Five.Island.df$Year)
#2018
Five.Island.df.18<-droplevels(subset(Five.Island.df, Year == "2018"))
Five.Island.m.18<-lm(Weight..g. ~ Length..mm., data = Five.Island.df.18)
summary(Five.Island.m.18)
#2019
Five.Island.df.19<-droplevels(subset(Five.Island.df, Year == "2019"))
Five.Island.m.19<-lm(Weight..g. ~ Length..mm., data = Five.Island.df.19)
summary(Five.Island.m.19)

# apply slope and intercept model to observed fall lengths
#
# subset for fall lengths (std runs aug-oct only)
# Use df2 above, split for Five.Island and by years
Five.Island.df2<-droplevels(subset(df2, Lake == "5 Island"))
Five.Island.df2.18<-droplevels(subset(Five.Island.df2, Year == "2018"))
Five.Island.df2.19<-droplevels(subset(Five.Island.df2, Year == "2019"))

# create expected weight column, 
est.18<-predict(Five.Island.m.18,Five.Island.df2.18)
est.19<-predict(Five.Island.m.19,Five.Island.df2.19)

# average every year.
average.est.18<-mean(est.18, na.rm = T)
average.est.19<-mean(est.19, na.rm = T)

# l-w regression from all coc data in each year
#
#    N.Twin Lake
#
#
N.Twin.df<-droplevels(subset(df, Lake == "N Twin"))
N.Twin.df<-subset(N.Twin.df, !is.na(Weight..g.))
summary(N.Twin.df$Weight..g.) # checking range
summary(N.Twin.df$Length..mm.) # checking range
N.Twin.df$Year<-as.factor(N.Twin.df$Year)
#2017
N.Twin.df.17<-droplevels(subset(N.Twin.df, Year == "2017"))
N.Twin.m.17<-lm(Weight..g. ~ Length..mm., data = N.Twin.df.17)
summary(N.Twin.m.17)
#2018
N.Twin.df.18<-droplevels(subset(N.Twin.df, Year == "2018"))
N.Twin.m.18<-lm(Weight..g. ~ Length..mm., data = N.Twin.df.18)
summary(N.Twin.m.18)
#2019
N.Twin.df.19<-droplevels(subset(N.Twin.df, Year == "2019"))
N.Twin.m.19<-lm(Weight..g. ~ Length..mm., data = N.Twin.df.19)
summary(N.Twin.m.19)

# apply slope and intercept model to observed fall lengths
#
# subset for fall lengths (std runs aug-oct only)
# Use df2 above, split for N.Twin and by years
N.Twin.df2<-droplevels(subset(df2, Lake == "N Twin"))
N.Twin.df2.17<-droplevels(subset(N.Twin.df2, Year == "2017"))
N.Twin.df2.18<-droplevels(subset(N.Twin.df2, Year == "2018"))
N.Twin.df2.19<-droplevels(subset(N.Twin.df2, Year == "2019"))

# create expected weight column, 
est.17<-predict(N.Twin.m.17,N.Twin.df2.17)
est.18<-predict(N.Twin.m.18,N.Twin.df2.18)
est.19<-predict(N.Twin.m.19,N.Twin.df2.19)

# average every year.
average.est.17<-mean(est.17, na.rm = T)
average.est.18<-mean(est.18, na.rm = T)
average.est.19<-mean(est.19, na.rm = T)

# l-w regression from all coc data in each year
#
#    S.Twin Lake
#
#
S.Twin.df<-droplevels(subset(df, Lake == "S Twin"))
S.Twin.df<-subset(S.Twin.df, !is.na(Weight..g.))
summary(S.Twin.df$Weight..g.) # checking range
summary(S.Twin.df$Length..mm.) # checking range
S.Twin.df$Year<-as.factor(S.Twin.df$Year)
#2017
S.Twin.df.17<-droplevels(subset(S.Twin.df, Year == "2017"))
S.Twin.m.17<-lm(Weight..g. ~ Length..mm., data = S.Twin.df.17)
summary(S.Twin.m.17)
#2018
S.Twin.df.18<-droplevels(subset(S.Twin.df, Year == "2018"))
S.Twin.m.18<-lm(Weight..g. ~ Length..mm., data = S.Twin.df.18)
summary(S.Twin.m.18)
#2019
S.Twin.df.19<-droplevels(subset(S.Twin.df, Year == "2019"))
S.Twin.m.19<-lm(Weight..g. ~ Length..mm., data = S.Twin.df.19)
summary(S.Twin.m.19)

# apply slope and intercept model to observed fall lengths
#
# subset for fall lengths (std runs aug-oct only)
# Use df2 above, split for S.Twin and by years
S.Twin.df2<-droplevels(subset(df2, Lake == "S Twin"))
S.Twin.df2.17<-droplevels(subset(S.Twin.df2, Year == "2017"))
S.Twin.df2.18<-droplevels(subset(S.Twin.df2, Year == "2018"))
S.Twin.df2.19<-droplevels(subset(S.Twin.df2, Year == "2019"))

# create expected weight column, 
est.17<-predict(S.Twin.m.17,S.Twin.df2.17)
est.18<-predict(S.Twin.m.18,S.Twin.df2.18)
est.19<-predict(S.Twin.m.19,S.Twin.df2.19)

# average every year.
average.est.17<-mean(est.17, na.rm = T)
average.est.18<-mean(est.18, na.rm = T)
average.est.19<-mean(est.19, na.rm = T)

# l-w regression from all coc data in each year
#
#    Silver Lake
#
#
Silver.df<-droplevels(subset(df, Lake == "Silver"))
Silver.df<-subset(Silver.df, !is.na(Weight..g.))
summary(Silver.df$Weight..g.) # checking range
summary(Silver.df$Length..mm.) # checking range
Silver.df$Year<-as.factor(Silver.df$Year)
#2018
Silver.df.18<-droplevels(subset(Silver.df, Year == "2018"))
Silver.m.18<-lm(Weight..g. ~ Length..mm., data = Silver.df.18)
summary(Silver.m.18)
#2019
Silver.df.19<-droplevels(subset(Silver.df, Year == "2019"))
Silver.m.19<-lm(Weight..g. ~ Length..mm., data = Silver.df.19)
summary(Silver.m.19)

# apply slope and intercept model to observed fall lengths
#
# subset for fall lengths (std runs aug-oct only)
# Use df2 above, split for Silver and by years
Silver.df2<-droplevels(subset(df2, Lake == "Silver"))
Silver.df2.18<-droplevels(subset(Silver.df2, Year == "2018"))
Silver.df2.19<-droplevels(subset(Silver.df2, Year == "2019"))

# create expected weight column, 
est.18<-predict(Silver.m.18,Silver.df2.18)
est.19<-predict(Silver.m.19,Silver.df2.19)

# average every year.
average.est.18<-mean(est.18, na.rm = T)
average.est.19<-mean(est.19, na.rm = T)

# l-w regression from all coc data in each year
#
#    Storm Lake
#
#
Storm.df<-droplevels(subset(df, Lake == "Storm"))
Storm.df<-subset(Storm.df, !is.na(Weight..g.))
summary(Storm.df$Weight..g.) # checking range
summary(Storm.df$Length..mm.) # checking range
Storm.df$Year<-as.factor(Storm.df$Year)
#2018
Storm.df.18<-droplevels(subset(Storm.df, Year == "2018"))
Storm.m.18<-lm(Weight..g. ~ Length..mm., data = Storm.df.18)
summary(Storm.m.18)
#2019
Storm.df.19<-droplevels(subset(Storm.df, Year == "2019"))
Storm.m.19<-lm(Weight..g. ~ Length..mm., data = Storm.df.19)
summary(Storm.m.19)

# apply slope and intercept model to observed fall lengths
#
# subset for fall lengths (std runs aug-oct only)
# Use df2 above, split for Storm and by years
Storm.df2<-droplevels(subset(df2, Lake == "Storm"))
Storm.df2.18<-droplevels(subset(Storm.df2, Year == "2018"))
Storm.df2.19<-droplevels(subset(Storm.df2, Year == "2019"))

# create expected weight column, 
est.18<-predict(Storm.m.18,Storm.df2.18)
est.19<-predict(Storm.m.19,Storm.df2.19)

# average every year.
average.est.18<-mean(est.18, na.rm = T)
average.est.19<-mean(est.19, na.rm = T)
######################################################################









#######################################################################
# average CPUE by lake and year
# using df2 from above

# need to aggregate data by lake and year, then also average catch per hour for all transects in that aggregation?
library(reshape2)
str(df2$On.Time..s.)
df2$On.Time..s.<-as.numeric(df2$On.Time..s.)
catch.df<-dcast(df2, Date+Lake+Year+Site.Transect+On.Time..s.+Species~ detect)

# good so far.
# Need to add 0 catches from 2018 ('dummy' species represents 0 catches in 2019)
## none for blue lake or five island. Other five lakes are here:
catch.df$Year<-as.factor(catch.df$Year)
str(catch.df)
catch.df$`1`[catch.df$Species == "dummy"]<-0
catch.df$Species[catch.df$Species == "dummy"]<-"COC"
#delete date column
catch.df<-droplevels(catch.df[,-1])
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
str(cpue.df)
groupwiseMean(CPUE~ Lake+Year,
              cpue.df)
# looking good, son!