# two boundaries for Qda bayesian classifier
mu1=0; s1=2; pi1=0.5
mu2=3; s2=1; pi2=0.5
curve(pi1*dnorm(x, mu1, s1) +pi2*dnorm(x, mu2, s2), -4, 7, 300 ,ylim=c(0,0.3))
curve(pi1*dnorm(x, mu1, s1), col=2, add=T)
curve(pi2*dnorm(x, mu2, s2), col=3, add=T)
pred1 = function(x) ((x-mu1)/s1)^2-2*log(pi1)+log(s1^2) <((x-mu2)/s2)^2-2*log(pi2)+log(s2^2)
curve(0.3*pred1(x), n=1001, lty=2, add=T)

curve(pi1*dnorm(x, mu1, s1) +pi2*dnorm(x, mu2, s2), 6, 8)
curve(pi1*dnorm(x, mu1, s1), col=2, add=T)
curve(pi2*dnorm(x, mu2, s2), col=3, add=T)