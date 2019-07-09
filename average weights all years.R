df<-read.csv("2018 and 2017 Compiled Data.csv",header=T)
summary(df)
summary(df$Lake)

blue<-droplevels(subset(df, Lake == "Blue"))
blue.c<-droplevels(subset(blue, Species == "COC"))
summary(blue.c$Weight..g.)



center<-droplevels(subset(df, Lake == "Center"))
center.c<-droplevels(subset(center, Species == "COC"))
summary(center.c$Weight..g.)
#
center.b<-droplevels(subset(center,Species == "BIB"))
summary(center.b$Weight..g.)


fiveI<-droplevels(subset(df, Lake == "5 Island"))
fiveI.c<-droplevels(subset(fiveI, Species == "COC"))
summary(fiveI.c$Weight..g.)
#
fiveI.b<-droplevels(subset(fiveI,Species == "BIB"))
summary(fiveI.b$Weight..g.)

NT<-droplevels(subset(df, Lake == "N. Twin"))
NT.c<-droplevels(subset(NT, Species == "COC"))
summary(NT.c$Weight..g.)
#
NT.b<-droplevels(subset(NT,Species == "BIB"))
summary(NT.b$Weight..g.)

ST<-droplevels(subset(df, Lake == "S. Twin"))
ST.c<-droplevels(subset(ST, Species == "COC"))
summary(ST.c$Weight..g.)
#
ST.b<-droplevels(subset(ST,Species == "BIB"))
summary(ST.b$Weight..g.)

Silver<-droplevels(subset(df, Lake == "Silver"))
Silver.c<-droplevels(subset(Silver, Species == "COC"))
summary(Silver.c$Weight..g.)
#
Silver.b<-droplevels(subset(Silver,Species == "BIB"))
summary(Silver.b$Weight..g.)

Storm<-droplevels(subset(df, Lake == "Storm"))
Storm.c<-droplevels(subset(Storm, Species == "COC"))
summary(Storm.c$Weight..g.)
#
Storm.b<-droplevels(subset(Storm,Species == "BIB"))
summary(Storm.b$Weight..g.)
