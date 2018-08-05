library(ISLR)
library(leaps) # regsubsets
library(glmnet)


##
## Lab 1: Subset Selection Methods
##

## Data Exploration
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))


## Data Cleaning
Hitters.old <- Hitters

Hitters <- na.omit(Hitters)


## Best Subset selection
##
## The summary of the regsubsets model reports which variables should
## be included in the best model with a given number of variables, up
## to nvmax.
regfit.full <- regsubsets(Salary ~ ., Hitters, nvmax=19)
summary(regfit.full)

reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq

## Let's plot indicators of fit to select the best model
par(mfrow=c(2,2))

plot(reg.summary$rss, xlab="Num of Variables", ylab="RSS", type="l")

plot(reg.summary$adjr2, xlab="Num of Variables", ylab="Adjusted R-squared", type="l")
max.adjr2 <- which.max(reg.summary$adjr2)
max.adjr2
points(max.adjr2, reg.summary$adjr2[max.adjr2], col="red", cex=2, pch=20)

plot(reg.summary$cp, xlab="Num ov Variables", ylab="Cp", type="l")
min.cp <- which.min(reg.summary$cp)
points(min.cp, reg.summary$cp[min.cp], col="red", cex=2, pch=20)

plot(reg.summary$bic, xlab="Num ov Variables", ylab="Bic", type="l")
min.bic <- which.min(reg.summary$bic)
points(min.bic, reg.summary$bic[min.bic], col="red", cex=2, pch=20)

## Or use built in plotting tools
##
## The top row of each plot shows the optimal model according to that
## statistic.
par(mfrow=c(2,2))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

## We can fetch the best model's coefficients
coef(regfit.full, which.min(reg.summary$bic))

## Forward and Backwards Stepwise Selection
##
## regsubsets has an argument method

regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~., data=Hitters, nvmax=5, method="backward")
summary(regfit.bwd)

## Choosing the right model using CV
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test <- !train
regfit.best <- regsubsets(Salary ~ ., data=Hitters[train,], nvmax=19)

test.mat <- model.matrix(Salary ~ ., data=Hitters[test,])

val.errors <- rep(NA, 19)
for (i in 1:19) {
    coef.i <- coef(regfit.best,id=i)
    pred <- test.mat[,names(coef.i)] %*% coef.i
    val.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}

##
## Lab 2: Ridge Regression and the Lasso
##
x <- model.matrix(Salary ~ ., Hitters)[,-1]
y <- Hitters$Salary


## glmnet takes an `alpha` parameter.
##   - alpha=0 :: ridge regression
##   - alpha=1 :: lasso
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)

## A coefficient matrix, 20x100 (coefficent x lambda)
coef(ridge.mod)

## Coefficients should be higher for small lambda (excluding intercept)
sum(abs(coef(ridge.mod)[-1,1])) # lamba = 10^10
sum(abs(coef(ridge.mod)[-1,100])) # lamba = 10^-2

predict(ridge.mod, s=50, type="coefficients")[1:20,]

## Test Set CV
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- -train
y.test <- y[test]


# Train our model
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)

## MSE of lambda = 4
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test)^2)
## 101037

## MSE of lambda = 10^10 (basically the null model)
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)
## 193253

## MSE of lambda = 0 (ordinary linear regression)
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,])
mean((ridge.pred - y.test)^2)
## 114724


## Selecting lambda using CV
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
## bestlam = 212

## MSE of lambda = CV selected lambda
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred - y.test)^2)
## 96015

## Final model
out <- glmnet(x, y, alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20,]

##
## The Lasso
##
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min

## MSE of lambda = CV best -- lasso version
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2)
## 100743

## Train with full data, show coefficients
out <- gmlnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:20,]
trunc(lasso.coef)
