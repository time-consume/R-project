dat<-read.csv("luxury.csv",header=T)
dat$MCSelling_price<-c(scale(dat$Selling_Price,center = T,scale = F))
lm.fit1<-lm(dat$Purchase_Intent~dat$MCSelling_price*dat$Discount_Offered,data=dat)
lm.fit2<-lm(dat$Purchase_Intent~dat$Selling_Price*dat$Discount_Offered,data=dat)
it1<-summary(lm.fit1)
it2<-summary(lm.fit2)
mean(dat$Selling_Price)
it1$coefficients
it2$coefficients
predict(lm.fit1,dat)
predict(lm.fit2,dat)
