setwd("~/Desktop/DS")
#require
require("aod")
require("ggplot2")
require("car")
require("Himsc")
require("rms")
require("QuantPsyc")
require("InformationValue")
#load data
library(readxl)
Pandora_Churn<-read_excel("Pandora Churn.xlsx")
attach(Pandora_Churn)
#summary data
head(Pandora_Churn)
str(Pandora_Churn)
summary(Pandora_Churn)
table(Pandora_Churn$Churn)
summary(Pandora_Churn)
table(Pandora_Churn$Churn)
table(Pandora_Churn$gender)
names(Pandora_Churn)
# two sample t-test
t.test(FavSongs_hour~Churn, data=Pandora_Churn)
t.test(Commercials_hour~Churn,data=Pandora_Churn)
#Histogram to check normality
par(mfrow=c(1:2))
hist(Pandora_Churn$FavSongs_hour)
hist(Pandora_Churn$Commercials_hour)
#Outlier
boxplot(Pandora_Churn$FavSongs_hour)
boxplot(Pandora_Churn$Commercials_hour)
#crosstabs for churn and gender
table<-xtabs(~Churn+gender)
table
summary(table)
#check correlation of the varaibles
cor(Pandora_Churn)
#logistic regression only intercept
logit<-glm(Churn~1,data=Pandora_Churn,family="binomial")
summary(logit)
ClassLog(logit,Churn,cut = 0.5)
#logistic regression
mylogit<-glm(Churn~FavSongs_hour+Commercials_hour+gender,data=Pandora_Churn,family = "binomial")
summary(mylogit)
#Wald test for logistic regression
wald.test(b=coef(mylogit),Sigma = vcov(mylogit),Terms = 1)
wald.test(b=coef(mylogit),Sigma = vcov(mylogit),Terms = 2)
wald.test(b=coef(mylogit),Sigma = vcov(mylogit),Terms = 3)
wald.test(b=coef(mylogit),Sigma = vcov(mylogit),Terms = 4)
#Odds ratio and confidence interval
exp(mylogit$coefficients)
exp(cbind(coef(mylogit),confint(mylogit)))
#improve or not
with(mylogit,null.deciance-deviance)
with(mylogit,df.null-df.residual)
#classification table for logistic model
ClassLog(mylogit,Churn,cut = 0.5)
#predict individual probility
pred<-predict(mylogit,type="response")
class<-ifelse(pred>0.5,"Yes","No")
class