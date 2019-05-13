df<-read.csv("2018 and 2017 Compiled Data.csv",header=T)
summary(df)


blue<-droplevels(subset(df, Lake == "Blue"))
blue.c<-droplevels(subset(blue, Species == "COC"))
summary(blue.c$Weight..g.)



center<-droplevels(subset(df, Lake == "Center"))
blue.c<-droplevels(subset(blue, Species == "COC"))
summary(blue.c$Weight..g.)
