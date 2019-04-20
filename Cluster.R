library(ISLR)
Hitters1 = Hitters[,1:7]
names(Hitters1); dim(Hitters1)
aa = kmeans(Hitters1, 3, nstart=10) ## K=3; run the algorithm 10 times
aa
str(aa)
all.equal(aa$tot.withinss, sum((Hitters1 - aa$centers[aa$cluster,])^2)) ## True
all.equal(aa$totss, sum((t(Hitters1) - apply(Hitters1,2,mean))^2)) ## True
# scale the variables
Hitters2 = scale(Hitters1)
aa = kmeans(Hitters1, 3, nstart=10)
bb = kmeans(Hitters2, 3, nstart=10)
table(aa$cluster, bb$cluster) ## quite different results!
#use two features too demonstrate
Hitters3 = Hitters1[,c(1,3)]
Hitters4 = scale(Hitters3)
aa = kmeans(Hitters3, 5, nstart=10)
bb = kmeans(Hitters4, 5, nstart=10)
table(aa$cluster, bb$cluster)
par(mfrow=c(1,2))
#As we can see from plot 1, the cluster mainly depend on AtBat
plot(Hitters3, col=aa$cluster); points(aa$centers, pch=17)
plot(Hitters4, col=bb$cluster); points(bb$centers, pch=17)


library(ISLR)
Hitters1 = Hitters[,1:7]
Hitters2 = scale(Hitters1) ## scaling the features
dist1 = dist(Hitters2) ## pairwise distance
dim(Hitters2) ## 322 x 7
str(dist1) ## 51681 = 322 * 321 / 2
sqrt(sum((Hitters2[1,]-Hitters2[2,])^2))

cluster1 = cutree(hclust(dist1), h=5.7) ## height 5.7
cluster2 = cutree(hclust(dist1), k=5) ## K=5
table(cluster1); table(cluster2)
table(cluster1, cluster2) ## same clustering
#Euclidean 
par(mfrow=c(1,1))
plot(hclust(dist1, method='complete'), labels=F, xlab='', main="Complete")
plot(hclust(dist1, method='average'), labels=F, xlab='', main="Average")
plot(hclust(dist1, method='single'), labels=F, xlab='', main="Single")
#1-pearson
dist2 = as.dist(1 - cor(t(Hitters2)))
plot(hclust(dist2, method='complete'), labels=F, xlab='', main="Complete")
plot(hclust(dist2, method='average'), labels=F, xlab='', main="Average")
plot(hclust(dist2, method='single'), labels=F, xlab='', main="Single")
#mixture modeling-more like lda qda except that we don't know the outcome


#NCI 
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
nci.labs = paste(1:64, nci.labs) ## add indices to labels for the plots
dim(nci.data) ## 64 x 6830
hist(nci.data)
sd.data = scale(nci.data) ## standardize the columns (genes)
data.dist = dist(sd.data)
hc1 = hclust(data.dist) ## complete linkage by default
plot(hc1, labels=nci.labs, main="Complete Linkage", xlab="", ylab="", sub="")
#abline(h=hc1$height[1:3], col='grey') ## add the first 3 heights
hc2 = hclust(data.dist, method="average") ## average linkage
plot(hc2, labels=nci.labs, main="Average Linkage", xlab="", ylab="", sub="")
hc3 = hclust(data.dist, method="single") ## single linkage
plot(hc3, labels=nci.labs, main="Single Linkage", xlab="", ylab="", sub="")

names(hc1)
hc1$order ## the order of observations to be drawn
hc1$height
dim(hc1$merge); hc1$merge
#negative means sample, positive means newly merged subset

