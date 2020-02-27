df<-read.csv("Hanna_CPUE_2019_Rawish.csv", header = T)
head(df)
str(df)
# change effing five island
df<-subset(df, Lake != "5 Island")

library(FSA)
headtail(df)

# talk out the plot
# X axis will be the transect numbers.
# Y axis is going to be the mean CPUE with CI error bars
# Boxplot format would be ideal
###############
#
#
# Also !note! that there's currently no map for silver. Five Island and Center only


library(ggplot2)
library(reshape2)
library(ggpmisc)
library(wesanderson)
library(gridExtra)
cpue2<-droplevels(subset(cpue2, Lake != "Silver"))
cpue2$Season<-ordered(cpue2$Season, c("Spring","Summer","Fall"))
cpue2$Species<-as.character(cpue2$Species)
cpue2$Species[cpue2$Species == "BIB"] <- "Bigmouth Buffalo"
cpue2$Species[cpue2$Species == "COC"] <- "Common Carp"
cpue2$Species<-as.factor(cpue2$Species)
cpue2$Lake<-as.character(cpue2$Lake)
cpue2$Lake[cpue2$Lake == "5 Island"] <- "Five Island Lake"
cpue2$Lake[cpue2$Lake == "Center"] <- "Center Lake"
cpue2$Lake<-as.factor(cpue2$Lake)
cpue2.b<-droplevels(subset(cpue2, Species == "Bigmouth Buffalo"))
cpue2.c<-droplevels(subset(cpue2, Species == "Common Carp"))


cpue5<-droplevels(subset(cpue5, Lake != "Silver"))
cpue5$Species<-as.character(cpue5$Species)
cpue5$Species[cpue5$Species == "BIB"] <- "Bigmouth Buffalo"
cpue5$Species[cpue5$Species == "COC"] <- "Common Carp"
cpue5$Species<-as.factor(cpue5$Species)
cpue5$Season<-ordered(cpue5$Season, c("Spring","Summer","Fall"))
cpue5$Trad.lower[cpue5$Trad.lower <= 0] <- 0

cpue5.b<-droplevels(subset(cpue5, Species == "Bigmouth Buffalo"))
cpue5.c<-droplevels(subset(cpue5, Species == "Common Carp"))




# plots

funnybone.b  <- ggplot(cpue2.b, aes(x = Site.Transect, y = cpue))+
                      geom_boxplot(stat = "boxplot",
                                   aes(fill = Season),
                                   position = "dodge2",
                                   size = 1.2)+
                    scale_fill_manual(values = wes_palette("Darjeeling1"))+
                    facet_grid(Lake~.)+
  theme(legend.position = c(.15,.3),legend.direction = "vertical",
        legend.title = element_text(size = 20),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        strip.text = element_text(size = 20),
        title = element_text(size = 14))+
  labs(title = "Buffalo CPUE Across Standard Transects",
       y = "Electrofishing CPUE (# Fish / Hour)",
       x = "Standard Electrofishing Transect")
                      
funnybone.c  <- ggplot(cpue2.c, aes(x = Site.Transect, y = cpue))+
  geom_boxplot(stat = "boxplot",
               aes(fill = Season),
               position = "dodge2",
               size = 1.2)+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  facet_grid(Lake~.)+
  theme(legend.position = c(.15,.3),legend.direction = "vertical",
        legend.title = element_text(size = 20),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        strip.text = element_text(size = 20),
        title = element_text(size = 14))+
  labs(title = "Carp CPUE Across Standard Transects",
       y = "Electrofishing CPUE (# Fish / Hour)",
       x = "Standard Electrofishing Transect")

grid.arrange(funnybone.b, funnybone.c, ncol=2)





kneebone.b <- ggplot(cpue5.b, aes(x = Season, y = Mean))+
                geom_point(size = 4)+
                geom_errorbar(aes(ymax = Trad.upper, ymin = Trad.lower), width = 0.3)+
                facet_grid(Lake~.)+
                scale_colour_manual(values = wes_palette("Darjeeling1"))+
  theme(legend.position = c(.2,.3),legend.direction = "vertical",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        strip.text = element_text(size = 20),
        title = element_text(size = 14))+
    labs(title = "Buffalo Electrofishing CPUE by Season",
         y = "Electrofishing CPUE (# Fish / Hour)",
         x = "Season")+
  ylim(c(0,60))

kneebone.c <- ggplot(cpue5.c, aes(x = Season, y = Mean))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymax = Trad.upper, ymin = Trad.lower), width = 0.3)+
  facet_grid(Lake~.)+
  scale_colour_manual(values = wes_palette("Darjeeling1"))+
  theme(legend.position = c(.1,.3),legend.direction = "vertical",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        strip.text = element_text(size = 20),
        title = element_text(size = 14))+
  labs(title = "Carp Electrofishing CPUE by Season",
       y = "Electrofishing CPUE (# Fish / Hour)",
       x = "Season")+
  ylim(c(0,60))



grid.arrange(kneebone.b, funnybone.b, ncol=2)
grid.arrange(kneebone.c, funnybone.c, ncol=2)
