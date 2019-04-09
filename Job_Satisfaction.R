#Load packages
require(car)
require(ggplot2)
require(jtools)
require(pequod)
#Read data
dat<-read.csv("EmployeeSat.csv")
names(dat)
#lm w/o interactions
lmbase<-lm(dat$Job_Satisfaction~dat$ValueCompensation+dat$age)
summary(lmbase)
#center
dat$MCcompensation<-scale(dat$ValueCompensation,center = TRUE,scale = F)
dat$MCage<-scale(dat$age,center = TRUE,scale = F)
#linear moderation
lmint<-lmres(dat$Job_Satisfaction~dat$ValueCompensation*dat$age,data = dat)
summary(lmint)
anova(lmbase,lmint)
#interaction plot
lmint1<-lmres(Job_Satisfaction~ValueCompensation+age,data = dat)
lmint2<-lmres(Job_Satisfaction~ValueCompensation*age,data = dat)
slopes<-simpleSlope(lmint2,pred="Job_Satisfaction",mod1 = "ValueCompensation")

