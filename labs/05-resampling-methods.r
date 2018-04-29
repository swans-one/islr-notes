library(ISLR)
library(boot)   # cv.glm

## 5.3.1 The validation set approach

set.seed(1)
train <- sample(392,196)

lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)

mse <- mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)

## 5.3.2 Leave One Out Cross-Validation

## LOOCV can be automatically computed for any generalized linear
## model using the glm and cv.glm functions.

## *Note*, with no `family` arugment, glm does linear regression

glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

## 5.3.3 k-Fold Cross-Validation

## For glm models, we can still use cv.glm with a K parameter

set.seed(17)
cv.error.10=rep(0, 10)
for (i in 1:10) {
    glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
    cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

## 5.3.4 The Bootstrap
