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
cpue2<-droplevels(subset(cpue2, Lake != "Silver"))
cpue2$Season<-ordered(cpue2$Season, c("Spring","Summer","Fall"))
cpue2$Species<-as.character(cpue2$Species)
cpue2$Species[cpue2$Species == "BIB"] <- "Bigmouth Buffalo"
cpue2$Species[cpue2$Species == "COC"] <- "Common Carp"
cpue2$Species<-as.factor(cpue2$Species)
funnybone  <- ggplot(cpue2, aes(x = Site.Transect, y = cpue))+
                      geom_boxplot(stat = "boxplot",
                                   aes(fill = Season),
                                   position = "dodge2",
                                   size = 1.5)+
                    scale_fill_manual(values = wes_palette("Darjeeling1"))+
                    facet_grid(Lake~Species)+
  theme(legend.position = c(.1,.8),legend.direction = "vertical",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"))+
  labs(title = "Seasonal Change in CPUE Across Standard Transects",
       y = "Electrofishing CPUE (# Fish / Hour)",
       x = "Standard Electrofishing Transect")
                      


# give me that
funnybone


cpue5<-droplevels(subset(cpue5, Lake != "Silver"))
cpue5$Species<-as.character(cpue5$Species)
cpue5$Species[cpue5$Species == "BIB"] <- "Bigmouth Buffalo"
cpue5$Species[cpue5$Species == "COC"] <- "Common Carp"
cpue5$Species<-as.factor(cpue5$Species)

cpue5$Season<-ordered(cpue5$Season, c("Spring","Summer","Fall"))
kneebone <- ggplot(cpue5, aes(x = Season, y = Mean))+
                geom_point(size = 4)+
                geom_errorbar(aes(ymax = Trad.upper, ymin = Trad.lower), width = 0.3)+
                facet_grid(Lake~Species)+
                scale_colour_manual(values = wes_palette("Darjeeling1"))+
  theme(legend.position = c(.1,.3),legend.direction = "vertical",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white",colour="grey50"), 
        axis.line = element_line(colour = "black"))+
    labs(title = "Electrofishing CPUE by Season",
         y = "Electrofishing CPUE (# Fish / Hour)",
         x = "Season")

# don't hit the
kneebone

library(gridExtra)
grid.arrange(kneebone,funnybone, ncol = 2)
