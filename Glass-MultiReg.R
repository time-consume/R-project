
#Reading CSV File 
glassdata<-read.csv("Glass_Mult_CSV.csv",header=T)
glassdata
pairs(~SALES+BLDG+AUTO,data=glassdata)
glassreg<-lm(SALES ~ BLDG+AUTO,data=glassdata)
#str(coefficients(glassreg))
summary(glassreg)
anova(glassreg)
#str(summary(glassreg))
#summary(glassreg)$r.squared
#summary(glassreg)$adj.r.squared
#summary(glassreg)$sigma
#str(anova(glassreg))
#sum(anova(glassreg)$Sum)
#anova(glassreg)$Mean[3]


#Prediction
data<-data.frame(BLDG=c(10,20,30), AUTO=c(7.5,7.5,7.5))
pred.int<-predict(glassreg,data,level=0.95, interval="prediction")
newglassdata<-cbind(data,pred.int)
newglassdata

coefficients(glassreg)
confint(glassreg,level=0.95)
cbind(coefficients(glassreg),confint(glassreg,level=0.95))

#Detecting Ouliers
glassreg$residuals
rstandard(glassreg)
#How many standard deviation from average Y(x), 2 is nearly 1.96, which is double-tail 0.95 significance
cook<-cooks.distance(glassreg)
cook
plot(cook,ylab="Cooks Distance")
points(2,cook[3],col='red')

#Testing Homoscedasticity
plot(glassreg$fitted.values, glassreg$residuals)
zres<-rstandard(glassreg)
plot(glassreg$fitted.values, zres)

#Testing Linearity
plot(glassdata$BLDG, zres)
plot(glassdata$AUTO, zres)


#Test for Normality
hist(glassreg$residuals)
qqnorm(glassreg$residuals)
qqline(glassreg$residuals)
shapiro.test(glassreg$residuals)

#Test of Independence
data<-data.frame(YEAR=c(1:17))
newglassdata<-cbind(glassdata,data)
#newglassdata
plot(newglassdata$YEAR, glassreg$residuals)

install.packages("lmtest")
library("lmtest")
dwtest(glassreg, alternative = "two.sided")

