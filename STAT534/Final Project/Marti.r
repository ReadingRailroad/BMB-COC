# Marti.r

library(rjags)
setwd("~/BMB-COC/STAT534/Final Project")

cpue <- read.csv('EcostatFinalCPUE.csv')

cpue.m <- jags.model(
  'Marti.bug',
  data=list(logPop=log(72140), sdlogPop=0.079, # log pouplation size and log sd of population size from data (sdlogpop becomes Tau) 
    n=dim(cpue)[1], # samples
    logCPUE=log(cpue$CPUE), # log-transformed CPUE from data 
    AirTemp=cpue$AirTemp, # air temp in celcius (Subract 20 in .bug file)
    Wind=cpue$Wind), # wind speed in mph, (subtract 10 in .bug file)
  n.chains=3, # number of simulation sequences
  n.adapt=100) # tells jags how much effort to spend in finding right resampling method
  
coef(cpue.m)

# sample the MCMC chains 1000 times as burn-in, ignore the samples. this gets thrown away
update(cpue.m, 1000)

cpue.j <- coda.samples(cpue.m, # model
  c('logN','b0','b1','b2','sd'), # parameters we want back
  1000) # number of samples

par(mar=c(3,3,0,0)+0.2, mgp=c(2,0.8,0))
plot(cpue.j)

summary(cpue.j)

# we want to plot:
## draw curve of estimated catchabaility as function of air temp and wind using betas
## uncertainty 
cpue.i <- coda.samples(cpue.m, # model
                       c('logcatch','b0','b1','b2','sd'), # parameters we want back
                       1000) # number of samples

par(mar=c(3,3,3,3)+0.2, mgp=c(2,0.8,0))
plot(cpue.i)

summary(cpue.i)


#using a ggplot2 analog to visualize model output
install.packages("mcmcplots")
library(mcmcplots)
library(ggplot2)
# test of package for b1
denplot(cpue.i, parms = c('b1', collapse = FALSE, auto.layout = TRUE))
traplot(cpue.i, parms = c('b1'))
autplot1(cpue.i[,"b1", drop = FALSE])

# extracting a parameter, plotting it's confidence region
# beta 1: air temp
varnames(cpue.i)
str(cpue.i)
beta1.out <- cbind(cpue.i[[1]][,2], cpue.i[[2]][,2], cpue.i[[3]][,2])
attributes(beta1.out) <- NULL
dens.beta1 <- density(beta1.out)
q25.beta1 <- quantile(beta1.out, .025)
q975.beta1 <- quantile(beta1.out, .975)
dd.beta1 <- with(dens.beta1, data.frame(x,y))

qplot(x, y, data = dd.beta1, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta1, x>q25.beta1 & x<q975.beta1),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)


# beta 2, slope of wind speed
varnames(cpue.i)
str(cpue.i)
beta2.out <- cbind(cpue.i[[1]][,3], cpue.i[[2]][,3], cpue.i[[3]][,3])
attributes(beta2.out) <- NULL
dens.beta2 <- density(beta2.out)
q25.beta2 <- quantile(beta2.out, .025)
q975.beta2 <- quantile(beta2.out, .975)
dd.beta2 <- with(dens.beta2, data.frame(x,y))

qplot(x, y, data = dd.beta2, geom = "line", ylab = "", xlab = "") +
  geom_ribbon(data = subset(dd.beta2, x>q25.beta2 & x<q975.beta2),
              aes(ymax = y), ymin = 0, fill = "red", colour = NA, alpha = 0.5)
# appears to be not significant


cpue.i.out<-summary(cpue.i)
varnames(cpue.i.out)
str(cpue.i.out)
linear <- data.frame(cpue.i.out[[1]][c(1:3),], cpue.i.out[[2]][c(1:3),c(1,5)])
str(linear)

catch.int<-cpue.i.out[[1]][1,1]
catch.slope<-cpue.i.out[[1]][2,1]


m <- cpue.i.out$statistics[,"Mean"]
l <- cpue.i.out$quantiles[,"2.5%"]
u <- cpue.i.out$quantiles[,"97.5%"]

dim(cpue)
length(m)

# new attempt

ggplot(cpue, aes(x = log(AirTemp), y = log(CPUE/68620))) +
  geom_point() +
  geom_abline(slope = catch.slope, intercept = catch.int)+
  xlim(1,4)+
  ylim(-8,-3)+
  geom_ribbon(aes(ymin = catch.int-l[1], ymax = catch.int+u[1]), alpha = 0.2)+ # can't get this to work
  theme_bw()

# plot of distribution of ElogCPUE in boxplots, overlaid with observed CPUE 
cpue.k <- coda.samples(cpue.m, 
                       c('ElogCPUE'), # parameters we want back
                       1000) # number of samples
summary(cpue.k)
# storing model outputas object
cpue.k.out<-summary(cpue.k)

cpue.plot.data<-cbind(cpue,cpue.k.out[[1]][c(1:26),],cpue.k.out[[2]][c(1:26),c(1:5)])
str(cpue.plot.data)

ggplot(cpue.plot.data, aes(x = Date, y = log(CPUE)))+
         geom_boxplot(aes(ymin = cpue.plot.data$`2.5%`, lower = cpue.plot.data$`25%`, 
                           middle = cpue.plot.data$Mean, 
                           upper = cpue.plot.data$`75%`, ymax = cpue.plot.data$`97.5%`),
  stat = "identity")+
  geom_point()+
  theme_bw()
#
cbind(log(cpue.plot.data$CPUE),cpue.plot.data$Mean)











cpue.l <- coda.samples(cpue.m, 
                       c('logPop'), # parameters we want back
                       1000) # number of samples
summary(cpue.l)

output.j<-do.call(rbind.data.frame(),cpue.j)
