setwd("/Users/yichenghu/Desktop/ADA_ID2")
dat<-read.csv("PRR.csv")
View(dat)
table<-as.data.frame(table(dat$UserId))
table<-table<-as.data.frame(table(table[,2]))
table
View(table)
for (m in 1:25) {
  table$accu[m]<-sum(table$Freq[m:25])
}

for (j in 1:24) {
  table$rent[1]<-1
  table$rent[j+1]<-table$accu[j+1]/table$accu[j]
}
write.csv(table,file="1a.csv")
dev.off()
par(c(1,1))
plot(table$accu,type = "b",col=2,pch=16,lty=4,cex=0.5)

length(unique(dat$UserId))
a<-matrix(nrow = 30,ncol = 1)
for (n in seq(1,30,1)) {
  s=0
  d=0
  for (i in unique(dat$UserId)) {
    if(nrow(dat[dat$UserId==i,])>=n){
      s=s+dat[dat$UserId==i,]$Total.Charges[n]
      d=d+1
    }
  }
  print(s/d)
  a[n,1]<-(s/d)
}
write.csv(a[,1],file = "basket.csv")
