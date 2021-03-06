library(class)
library(ISLR)
names(Smarket)
train<-(Year<2005)
train.X<-cbind(Lag1,Lag2)[train,]
test.X<-cbind(Lag1,Lag2)[!train,]
train.Direction<-Direction[train]
test.Direction<-Direction[!train]
set.seed(1)
knn.pred<-knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,test.Direction)
mean(knn.pred==test.Direction)
