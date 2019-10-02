
## Age comparison plots for North Twin Lake
#
# Buffalo and Carp
#
#
age<-read.csv("Marcus/BuffaloAges_Compiled2.csv",header=T)
summary(age)
head(age)
str(age)
##############################################################

pairs(age[,-c(1:12,18:22)])
pairs(age[,-c(1:7,13:22)])
pairs(age[,-c(1:17)])

# Dorsal Spines
# Pectoral v Dorsal
p1<-ggplot(age, aes(carp,x=Marcus_Pectoral, y=Marcus_Dorsal))+
  geom_point(size=5, alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0)+
  labs(y = "Reader 1",
       x = "",
       title ="Dorsal Spines")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)+
  theme(plot.title = element_text(hjust = 0.5))

p1

model<-lm(Marcus_Dorsal~-1 + Marcus_Pectoral, data = age)
summary(model)
confint(model)





# Pectoral v sectioned otolith
p2<-ggplot(age, aes(x=Marcus_Pectoral, y=Marcus_SectOto))+
  geom_point(size=5, alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0)+
  labs(y = "",
       x="",
       title = "Sectioned Otoliths")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)+
  theme(plot.title = element_text(hjust = 0.5))

p2

model<-lm(Marcus_SectOto~Marcus_Pectoral, data = age)
summary(model)
confint(model)

# Pectoral v cracked otlith
p3<-ggplot(age, aes(x=Marcus_Pectoral, y=Marcus_CrackOto))+
  geom_point(size=5, alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0)+
  labs(y = "",
       x = "",
       title = "Cracked and Sanded Otoliths")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)+
  theme(plot.title = element_text(hjust = 0.5))

p3

model<-lm(Marcus_CrackOto~Marcus_Pectoral, data = age)
summary(model)
confint(model)


##############################################################


# Drake


# dorsal
p4<-ggplot(age, aes(carp,x=Drake_Pectoral, y=Drake_Dorsal))+
  geom_point(size=5, alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0)+
  labs(y = "Reader 2",
       x = "")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)
p4

model<-lm(Drake_Dorsal~Drake_Pectoral, data = age)
summary(model)
confint(model)



# sectioned otlith
p5<-ggplot(age, aes(x=Drake_Pectoral, y=Drake_SectOto))+
  geom_point(size=5, alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0)+
  labs(y = "",
       x = "")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)
p5

model<-lm(Drake_SectOto~Drake_Pectoral, data = age)
summary(model)
confint(model)

# cracked otolith
p6<-ggplot(age, aes(x=Drake_Pectoral, y=Drake_Crackoto))+
  geom_point(size=5, alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0)+
  labs(y = "",
       x = "")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)
p6

model<-lm(Drake_Crackoto~Drake_Pectoral, data = age)
summary(model)
confint(model)

##############################################################

# Marty

# Dorsal
p7<-ggplot(age, aes(carp,x=Marty_Pectoral, y=Marty_Dorsal))+
  geom_point(size=5, alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "Pectoral Age",
       y = "Reader 3")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)
p7

model<-lm(Marty_Dorsal~Marty_Pectoral, data = age)
summary(model)
confint(model)

# sectioned otoliths
p8<-ggplot(age, aes(x=Marty_Pectoral, y=Marty_SectOto))+
  geom_point(size=5, alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0)+
  geom_smooth(method = "lm")+
  labs(x = "Pectoral Age",
       y = "")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)
p8

model<-lm(Marty_SectOto~Marty_Pectoral, data = age)
summary(model)
confint(model)

# cracked otlith
p9<-ggplot(age, aes(x=Marty_Pectoral, y=Marty_Crackoto))+
  geom_point(size=5, alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0)+
  geom_smooth(method="lm")+
  labs(x = "Pectoral Age",
       y = "")+
  theme_classic()+
  xlim(0,20)+
  ylim(0,20)
p9

model<-lm(Marty_Crackoto~Marty_Pectoral, data = age)
summary(model)
confint(model)

##############################################################

grid.arrange(p1,p2,p3,
             p4,p5,p6,
             p7,p8,p9,
             ncol=3)

