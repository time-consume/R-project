library(glmnet)
library(ISLR) ## for the Hitters dataset
Hitters = na.omit(Hitters); dim(Hitters) # 263, 20
x = model.matrix(Salary ~ ., Hitters)[,-1] # remove the "intercept" column
y = Hitters$Salary
x = scale(x)

ridge.mod = glmnet(x, y, alpha=0) # over a grid of lambda values
names(ridge.mod)
plot(ridge.mod, xvar='lambda')

cv.out = cv.glmnet(x, y, alpha=0)
cv.out = cv.glmnet(x, y, alpha=0,lambda = seq(0,20,0.05))
names(cv.out)
cv.out$lambda.min; cv.out$lambda.1se
with(cv.out, lambda.min == lambda[which.min(cvm)])
plot(cv.out)
