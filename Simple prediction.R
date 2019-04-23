setwd("/Users/yichenghu/Desktop/ADA_ID2")
dat<-read.csv("ABC.csv",header = T)
dat<-dat[,c(2,3,4,5,6)]
#set prediction
Price<-c(150,160,170,180,150,160,170,180,150,160,170,180,150,160,170,180)
Promo<-c(0,0,1,1,0,1,1,0,1,1,0,0,1,0,0,1)
Message<-c(0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,0)
#Response.1<-rep(numeric(0),16)
#Response.2<-rep(numeric(0),16)
pred<-data.frame(Price,Promo,Message)
#Logistic Regression
glm.fit<-glm(Response.1~Promo+Message+Price,family = "binomial",data = dat)
glm.fit$coefficients
exp(glm.fit$coefficients)-1
predict(glm.fit,pred)
exp(predict(glm.fit,pred))
#linear regression
lm.fit<-lm(Response.2~Promo+Message+Price,data = dat)
summary(lm.fit)
predict(lm.fit,pred)
pred$predict<-predict(lm.fit,pred)

