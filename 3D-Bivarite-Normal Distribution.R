mu1=c(0,0); s1=1.2; pi1=0.5
mu2=c(3,2); s2=1; pi2=0.5
grid<-seq(-5,5,.01)
x1grid = rep(grid, length(grid))
x2grid = rep(grid, each=length(grid))
d1 = pi1 * dnorm(x1grid, mu1[1], s1) * dnorm(x2grid, mu1[2], s1)
d2 = pi2 * dnorm(x1grid, mu2[1], s2) * dnorm(x2grid, mu2[2], s2)
library(rgl)
plot3d(x1grid, x2grid, d1, col='lightgrey') ## class 1 distribution
plot3d(x1grid, x2grid, d2, col='grey', add=T) ## class 2 distribution
