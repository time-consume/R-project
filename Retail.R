library(aod)
library(ggplot2)
library(car)
library(Hmisc)
library(rms)
library(ResourceSelection)
library(QuantPsyc)
library(InformationValue)
library(data.table)
setwd("/Users/boris/Desktop/444\ Predict\ Model/Logistic\ HW")
library(readxl)
dat <- read_excel("Retailer Pregnant Data (1).xlsx")
dat <- as.data.table(dat)
#Delete X__1 column
dat[, X__1 := NULL]
#Create Male and Female variable
dat[, Male := NA ]
for(i in 1:1000){
  if(dat$`Implied Gender`[i] == "M"){
    dat$Male[i] = 1
  }else{
    dat$Male[i] = 0
  }
}
dat[, Female := NA ]
for(i in 1:1000){
  if(dat$`Implied Gender`[i] == "F"){
    dat$Female[i] = 1
  }else{
    dat$Female[i] = 0
  }
}
#Create Home and APT variable
dat[, Home := NA ]
for(i in 1:1000){
  if(dat$`Home/Apt/ PO Box`[i] == "H"){
    dat$Home[i] = 1
  }else{
    dat$Home[i] = 0
  }
}
dat[, APT := NA ]
for(i in 1:1000){
  if(dat$`Home/Apt/ PO Box`[i] == "A"){
    dat$APT[i] = 1
  }else{
    dat$APT[i] = 0
  }
}
#Truncated dataset, exclude Implied Gender and Home/Apt/ PO Box variables#
RP<-subset(dat, select = -c(`Implied Gender`, `Home/Apt/ PO Box`))
str(RP)
summary(RP)

#Preliminary Analysis 
#crosstabs for PREGNANT and predictors#
table1<- xtabs(~PREGNANT+`Pregnancy Test`, data=RP)
print(table1)
summary(table1) #Significance

table2<- xtabs(~PREGNANT+`Birth Control`, data=RP)
print(table2)
summary(table2) #Significance

table3<- xtabs(~PREGNANT+`Feminine Hygiene`, data=RP)
print(table3)
summary(table3) #Significance

table4<- xtabs(~PREGNANT+`Folic Acid`, data=RP)
print(table4)
summary(table4) #Significance

table5<- xtabs(~PREGNANT+`Prenatal Vitamins`, data=RP)
print(table5)
summary(table5) #Significance

table6<- xtabs(~PREGNANT+`Prenatal Yoga`, data=RP)
print(table6)
summary(table6) #Significance

table7<- xtabs(~PREGNANT+`Body Pillow`, data=RP)
print(table7)
summary(table7) #Significance

table8<- xtabs(~PREGNANT+`Ginger Ale`, data=RP)
print(table8)
summary(table8) #Significance

table9<- xtabs(~PREGNANT+`Sea Bands`, data=RP)
print(table9)
summary(table9) #Significance

table10<- xtabs(~PREGNANT+`Stopped buying ciggies`, data=RP)
print(table10)
summary(table10) #Significance

table11<- xtabs(~PREGNANT+`Cigarettes`, data=RP)
print(table11)
summary(table11) #Significance

table12<- xtabs(~PREGNANT+`Smoking Cessation`, data=RP)
print(table12)
summary(table12) #Significance

table13<- xtabs(~PREGNANT+`Stopped buying wine`, data=RP)
print(table13)
summary(table13) #Significance

table14<- xtabs(~PREGNANT+`Wine`, data=RP)
print(table14)
summary(table14) #Significance

table15<- xtabs(~PREGNANT+`Maternity Clothes`, data=RP)
print(table15)
summary(table15) #Significance

table16<- xtabs(~PREGNANT+`Male`, data=RP)
print(table16)
summary(table16) #Significance

table17<- xtabs(~PREGNANT+`Female`, data=RP)
print(table17)
summary(table17) #Significance

table18<- xtabs(~PREGNANT+`Home`, data=RP)
print(table18)
summary(table18) #Not Significance

table19<- xtabs(~PREGNANT+`APT`, data=RP)
print(table19)
summary(table19) #Not Significance

#correlation matrix#
rcorr(as.matrix(RP))

##Logistic Regression for Model w/No Predictors#
mylogitbase<-glm(PREGNANT~1,data = RP, family = "binomial")
summary(mylogitbase)

#Wald Test for Base Model#
wald.test(b = coef(mylogitbase), Sigma = vcov(mylogitbase), Terms = 1)

#Classification Table for Base Model#
ClassLog(mylogitbase, RP$PREGNANT, cut = .5)

##Logistic Regression with all predictors
mylogit_all<-glm(PREGNANT~`Pregnancy Test`+`Birth Control`+`Feminine Hygiene`+`Folic Acid`
             +`Prenatal Vitamins`+`Prenatal Yoga`+`Body Pillow`+`Ginger Ale`
             +`Sea Bands`+`Stopped buying ciggies`+Cigarettes+`Smoking Cessation`
             +`Stopped buying wine`+`Cigarettes`+`Smoking Cessation`+`Stopped buying wine`
             +Wine+`Maternity Clothes`+Male+Female+Home+APT, 
             data = RP, family = "binomial")
summary(mylogit_all)

##Logistic Regression with significant predictors
mylogit<-glm(PREGNANT~`Pregnancy Test`+`Birth Control`+`Feminine Hygiene`+`Folic Acid`
                 +`Prenatal Vitamins`+`Prenatal Yoga`+`Ginger Ale`
                 +`Stopped buying ciggies`+Cigarettes+`Smoking Cessation`
                 +`Stopped buying wine`+`Cigarettes`+`Smoking Cessation`+`Stopped buying wine`
                 +Wine+`Maternity Clothes`, 
                 data = RP, family = "binomial")
summary(mylogit)

#Wald Tests for Logistic Regression Model#
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 1)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 2)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 3)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 5)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 6)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 7)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 8)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 9)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 10)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 11)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 12)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 13)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 14)

#Hosmer-Lemeshow Goodness (Badness) of Fit Test, g sets the n of steps#
hl<-hoslem.test(RP$PREGNANT, fitted(mylogit), g=10)
hl

#compare fit of model with predictors to fit of model with no predictor#
with(mylogit, null.deviance - deviance)

with(mylogit, df.null - df.residual)

with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual,
                     lower.tail = FALSE))

#Pseudo R-squared values#
library("DescTools")

PseudoR2(mylogit, which = "all")

#Classification Table for Model#
ClassLog(mylogit, RP$PREGNANT, cut = .5)

#Create individual probabilities#
predict(mylogit, type="response")
RP$PredProb<-predict(mylogit, type="response")

trn_pred <- ifelse(predict(mylogit, type = "response") > 0.5, "Yes", "No")
RP[, pred := NA]
for(i in 1:1000){
  RP$pred[i] <- trn_pred[i]
}

#create odds-ratios#
exp(coef(mylogit))

#odds ratio and 95% CI#
exp(cbind(OR = coef(mylogit), confint(mylogit)))
#No CI includes 1. 

#ROC and check classification value#
predicted <- plogis(predict(mylogit, RP))
plotROC(RP$PREGNANT, predicted)

#Get optimum cutoff value#
optCutOff <- optimalCutoff(RP$PREGNANT, predicted) 
optCutOff

#Classification Table for Model with optimum cutoff#
ClassLog(mylogit, RP$PREGNANT, cut = optCutOff)

#Stepwise
null<-glm(PREGNANT ~ 1,data=RP)
full<-glm(PREGNANT~`Pregnancy Test`+`Birth Control`+`Feminine Hygiene`+`Folic Acid`
          +`Prenatal Vitamins`+`Prenatal Yoga`+`Body Pillow`+`Ginger Ale`
          +`Sea Bands`+`Stopped buying ciggies`+Cigarettes+`Smoking Cessation`
          +`Stopped buying wine`+`Cigarettes`+`Smoking Cessation`+`Stopped buying wine`
          +Wine+`Maternity Clothes`+Male+Female+Home+APT,data=RP)
stepreg<-step(null,scope=list(lower=null,upper=full),direction="both")
summary(stepreg) # Not a good result

