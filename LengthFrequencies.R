# 2018 length frequency histograms. 2017 plots made in Initial Twin lake plots.Rmd

# Prepare to bring in 2017 data and arrange so compare coc vs. bib in both years for one lake

packages<-function(x, repos="http://cran.r-project.org", ...){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x, repos=repos, ...)
    require(x,character.only=TRUE)
  }
}

packages(vegan)
packages(ggplot2)
packages(reshape2)
packages(nnet)
packages(multcomp)
packages(plyr)
packages(rv)
packages(arm)
packages(lattice)
packages(tikzDevice)
packages(gridExtra)

###########################################################################################################
#
#
#
# 2017 First
#
#
# 2018 Below
#
#
###########################################################################################################

########################################################
#
#
# Twin Lakes
#
#
########################################################

df <- read.csv("C:\\Users/M_Sim/OneDrive/IOWA/Iowa State University/CarpR/TwinLakes/TwinLakesLength2017.csv")

summary(df)
# Keeping units of data sheets, although I would prefer cm to inches

NT.B<-subset(df,df$Lake=="N Twin" & df$Species == "BIB")
NT.C<-subset(df,df$Lake=="N Twin" & df$Species == "COC")
ST.B<-subset(df,df$Lake=="S Twin" & df$Species == "BIB")
ST.C<-subset(df,df$Lake=="S Twin" & df$Species == "COC")

North.B.2017<-ggplot(NT.B, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(
    breaks=1:2,
    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35)) +
  labs(x="Length (Inches)", y="Frequency",title="2017 North Twin Lake: Buffalo")

North.C.2017<-ggplot(NT.C, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(
    breaks=1:2,
    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2017 North Twin Lake: Carp")

South.B.2017<-ggplot(ST.B, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35)) +
  labs(x="Length (Inches)", y="Frequency",title="2017 South Twin Lake: Buffalo")

South.C.2017<-ggplot(ST.C, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35)) +
  labs(x="Length (Inches)", y="Frequency",title="2017 South Twin Lake: Carp")

#####################################################
#
#
# Center Lake
#
#
#####################################################
CL <- read.csv("C:\\Users/M_Sim/OneDrive/IOWA/Iowa State University/CarpR/CenterLake/CenterLake2017.csv",header=TRUE)

summary(CL)
CL$TL.in.[CL$TL.in.=="."]<-NA
CL$TL.in.[CL$TL.in.=="0.0"]<-NA
CL$TL.in.<-as.numeric(as.character(CL$TL.in.))
# Keeping units of data sheets, although I would prefer cm to inches
CL$Spp<-as.character(CL$Spp)
CL$Spp[CL$Spp == "CAP"]<-"COC"
CL$Spp<-as.factor(CL$Spp)
CL<-na.omit(CL)
summary(CL)

CL.B<-subset(CL,CL$Spp == "BIB")
CL.C<-subset(CL,CL$Spp == "COC")


Center.B.2017<-ggplot(CL.B, aes(x=TL.in., fill=Spp)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2017 Center Lake: Buffalo")

Center.C.2017 <-ggplot(CL.C, aes(x=TL.in., fill=Spp)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2017 Center Lake: Carp")


#############################################
#
# Blue Lake
#
#############################################
BL <- read.csv("C:\\Users/M_Sim/OneDrive/IOWA/Iowa State University/CarpR/BlueLake/BL_Carp_Lengths.csv",header=TRUE)
BL$Spp<-"COC"
BL$Spp<-as.factor(BL$Spp)
summary(BL)

Blue.2017 <-ggplot(BL, aes(x=Length..in., fill=Spp)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2017 Blue Lake: Carp")



##########################################################################################################
#
#
#
#
#     2018 now
#
#
#
#
###########################################################################################################


########################################################
#
#
# Twin Lakes
#
#
########################################################

df <- read.csv("C:\\Users/M_Sim/OneDrive/IOWA/Iowa State University/CarpR/July2018_ALW.csv")

summary(df)
# changing mm to in (consistency with annual report, etc.)
df$Length..inches.<-df$TL.mm*0.0393701

NT.B<-subset(df,df$Lake=="N Twin" & df$Species == "BIB")
NT.B<-subset(NT.B,!is.na(NT.B$Length..inches.))
NT.C<-subset(df,df$Lake=="N Twin" & df$Species == "COC")
NT.C<-subset(NT.C,!is.na(NT.B$Length..inches.))
ST.B<-subset(df,df$Lake=="S Twin" & df$Species == "BIB")
ST.B<-subset(ST.B,!is.na(NT.B$Length..inches.))
ST.C<-subset(df,df$Lake=="S Twin" & df$Species == "COC")
ST.C<-subset(ST.C,!is.na(NT.B$Length..inches.))

North.B.2018<-ggplot(NT.B, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(
    breaks=1:2,
    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35)) +
  labs(x="Length (Inches)", y="Frequency",title="2018 North Twin Lake: Buffalo")

North.C.2018<-ggplot(NT.C, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(
    breaks=1:2,
    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 North Twin Lake: Carp")

South.B.2018<-ggplot(ST.B, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35)) +
  labs(x="Length (Inches)", y="Frequency",title="2018 South Twin Lake: Buffalo")

South.C.2018<-ggplot(ST.C, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35)) +
  labs(x="Length (Inches)", y="Frequency",title="2018 South Twin Lake: Carp")

#####################################################
#
#
# Center Lake
#
#
#####################################################

CL.B<-subset(df,df$Lake=="Center" & df$Species == "BIB")
CL.C<-subset(df,df$Lake=="Center" & df$Species == "COC")


Center.B.2018<-ggplot(CL.B, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 Center Lake: Buffalo")

Center.C.2018<-ggplot(CL.C, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 Center Lake: Carp")


#############################################
#
# Blue Lake
#
#############################################

Blue.C<-subset(df,df$Lake=="Blue" & df$Species == "COC")

Blue.2018<-ggplot(Blue.C, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 Blue Lake: Carp")

#############################################
#
# Five Island
#
#############################################

FI.B<-subset(df,df$Lake=="Five Island" & df$Species == "BIB")
FI.C<-subset(df,df$Lake=="Five Island" & df$Species == "COC")


FI.B.2018<-ggplot(FI.B, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 Five Island Lake: Buffalo")

FI.C.2018<-ggplot(FI.C, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 Five Island Lake: Carp")


#############################################
#
# Silver
#
#############################################

Silver.B<-subset(df,df$Lake=="Silver" & df$Species == "BIB")
Silver.C<-subset(df,df$Lake=="Silver" & df$Species == "COC")


Silver.B.2018<-ggplot(Silver.B, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 Silver Lake: Buffalo")

Silver.C.2018<-ggplot(Silver.C, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 Silver Lake: Carp")

#############################################
#
# Storm
#
#############################################

Storm.B<-subset(df,df$Lake=="Storm" & df$Species == "BIB")
Storm.C<-subset(df,df$Lake=="Storm" & df$Species == "COC")


Storm.B.2018<-ggplot(Storm.B, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 Storm Lake: Buffalo")

Storm.C.2018<-ggplot(CL.C, aes(x=Length..inches., fill=Species)) +
  geom_histogram(binwidth=0.5, colour="black", position="dodge") +
  scale_fill_manual(breaks=1:2, 
                    values=c("cornflowerblue","goldenrod")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0,35))+
  labs(x="Length (Inches)", y="Frequency",title="2018 Storm Lake: Carp")





grid.arrange(Center.C.2017,Center.B.2017,
             Center.C.2018,Center.B.2018,
             ncol=2,nrow=2)

grid.arrange(North.C.2017,North.B.2017,
             North.C.2018,North.B.2018,
             ncol=2,nrow=2)
             
grid.arrange(South.C.2017,South.B.2017,
             South.C.2018,South.B.2018,
             ncol=2,nrow=2)

grid.arrange(Blue.2017,Blue.2018,
             ncol=1,nrow=2)

grid.arrange(Silver.C.2018,Silver.B.2018,
             Storm.C.2018,Storm.B.2018,
             FI.C.2018,FI.B.2018,
             ncol=2,nrow=3)




