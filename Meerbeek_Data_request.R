# meerbeek data request January 23, 2020
install.packages("openair")
library(openair)
library(rcompanion)
df<-read.csv("2017-2020 Compiled Data.csv")
str(df)

# only carp
df<-droplevels(subset(df, Species == "COC"))

# only electrofishing
levels(df$Gear)
df<-droplevels(subset(df, Gear == "Electroshocking"))

# August - October only
df$Date<-as.Date(df$Date, "%Y-%m-%d")
str(df$Date)
df$Month<-factor(format(df$Month, "%m"))
df<-droplevels(subset(df, Month == "08" |
                          Month == "09" |
                          Month == "10"))

# standard runs only:
levels(df$Site.Transect)
df<-droplevels(subset(df, Site.Transect == "Std 1" |
                          Site.Transect == "Std 2" |
                          Site.Transect == "Std 3" |
                          Site.Transect == "Std 4" |
                          Site.Transect == "Std 5" |
                          Site.Transect == "Std 6" |
                          Site.Transect == "Std 7" |
                          Site.Transect == "Std 9"))


# average weight by lake and year
groupwiseMean(Weight..g.~Lake + Year,
              data = df)

# average CPUE by weight and year 