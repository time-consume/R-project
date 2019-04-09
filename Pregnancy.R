#Activate packages#
library(aod)
library(ggplot2)
library(car)
library(Hmisc)
library(rms)
library(ResourceSelection)
library(QuantPsyc)
library(InformationValue)
library(DescTools)
#read data
dat<-read.csv("Pregnant.csv",header = T)
names(dat)
dim(dat)
#create dummy variables
dat$Male<-rep(0,1000)
dat$Male[dat$Implied.Gender=="M"]=1
dat$Female<-rep(0,1000)
dat$Female[dat$Implied.Gender=="F"]=1
dat$Home<-rep(0,1000)
dat$Home[dat$Home.Apt..PO.Box=="H"]=1
dat$Apt<-rep(0,1000)
dat$Apt[dat$Home.Apt..PO.Box=="A"]=1
#Modify dataset
MD<-subset(dat,select = -c(Implied.Gender,Home.Apt..PO.Box,X))
names(MD)
cor(MD)
#crosstabs for Loan_Status and predictors#
attach(MD)
#crosstabs for PREGNANT and predictors#
table1<- xtabs(~PREGNANT+Pregnancy.Test, data=MD)
print(table1)
summary(table1) #Significance

table2<- xtabs(~PREGNANT+Birth.Control, data=MD)
print(table2)
summary(table2) #Significance

table3<- xtabs(~PREGNANT+Feminine.Hygiene, data=MD)
print(table3)
summary(table3) #Significance

table4<- xtabs(~PREGNANT+Folic.Acid, data=MD)
print(table4)
summary(table4) #Significance

table5<- xtabs(~PREGNANT+Prenatal.Vitamins, data=MD)
print(table5)
summary(table5) #Significance

table6<- xtabs(~PREGNANT+Prenatal.Yoga, data=MD)
print(table6)
summary(table6) #Significance

table7<- xtabs(~PREGNANT+Body.Pillow, data=MD)
print(table7)
summary(table7) #Significance

table8<- xtabs(~PREGNANT+Ginger.Ale, data=MD)
print(table8)
summary(table8) #Significance

table9<- xtabs(~PREGNANT+Sea.Bands, data=MD)
print(table9)
summary(table9) #Significance

table10<- xtabs(~PREGNANT+Stopped.buying.ciggies, data=MD)
print(table10)
summary(table10) #Significance

table11<- xtabs(~PREGNANT+Cigarettes, data=MD)
print(table11)
summary(table11) #Significance

table12<- xtabs(~PREGNANT+Smoking.Cessation, data=MD)
print(table12)
summary(table12) #Significance

table13<- xtabs(~PREGNANT+Stopped.buying.wine, data=MD)
print(table13)
summary(table13) #Significance

table14<- xtabs(~PREGNANT+Wine, data=MD)
print(table14)
summary(table14) #Significance

table15<- xtabs(~PREGNANT+Maternity.Clothes, data=MD)
print(table15)
summary(table15) #Significance

table16<- xtabs(~PREGNANT+Male, data=MD)
print(table16)
summary(table16) #Significance

table17<- xtabs(~PREGNANT+Female, data=MD)
print(table17)
summary(table17) #Significance

table18<- xtabs(~PREGNANT+Home, data=MD)
print(table18)
summary(table18) #Not Significance

table19<- xtabs(~PREGNANT+Apt, data=MD)
print(table19)
summary(table19) #Not Significance

#stepwwise regression/null.full.with significant variable
null<-glm(PREGNANT~1,family = "binomial")
summary(null)
full<-glm(PREGNANT~.,data = MD,family = "binomial")
summary(full)
ClassLog(full,PREGNANT,cut = 0.5)
glm.fit<-glm(PREGNANT~.-Male-Female-Home-Apt-Sea.Bands-Body.Pillow,data = MD,family = "binomial")
summary(glm.fit)
ClassLog(glm.fit,PREGNANT,cut = 0.5)
anova(null,glm.fit)
anova(full,glm.fit)
par(mfrow=c(2,2))
plot(glm.fit)
#Independece,normality,but with some high-leverage points#
par(mfrow=c(2,2))
plot(glm.fit)
#Hosmer-Lemeshow Goodness (Badness) of Fit Test, g sets the n of steps#
hl<-hoslem.test(PREGNANT, glm.fit$fitted.values, g=10)
hl
#Pseudo R-squared values# 
PseudoR2(glm.fit, which = "all")
#create odds-ratios#
exp(coef(glm.fit))

#odds ratio and 95% CI#
exp(cbind(OR = coef(glm.fit), confint(glm.fit)))
#No CI includes 1. 

#ROC and check classification value#
predicted <- plogis(predict(glm.fit, MD))
plotROC(PREGNANT, predicted)

#Get optimum cutoff value#
optCutOff <- optimalCutoff(PREGNANT, predicted) 
optCutOff

#Classification Table for Model with optimum cutoff#
ClassLog(glm.fit, PREGNANT, cut = optCutOff)
#Predicted value based on new cutoff value#
glm.predict<-predict()
#LDA
library(MASS)
lda.fit<-lda(PREGNANT~.,data=MD)
predlda<-predict(lda.fit,MD)
mean(predlda$class==PREGNANT)
#QDA
qda.fit<-qda(PREGNANT~.,data = MD)
predqda<-predict(qda.fit,MD)
mean(predqda$class==PREGNANT)
#KNN 

