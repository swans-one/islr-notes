##
## 8. Simple linear regressions on Auto
##

## a)
auto.fit.8 <- lm(mpg ~ horsepower, data=Auto)
summary(auto.fit.8)

## i) Yes
## ii) A very robust relationship (p << .001)
## iii) Negative
## iv)
predict(auto.fit.8, data.frame(horsepower=c(98)), interval="confidence")
predict(auto.fit.8, data.frame(horsepower=c(98)), interval="prediction")

## b)
plot(Auto$horsepower, Auto$mpg)
abline(auto.fit.8, col="red")

## c)
par(mfrow=c(2,2))
plot(auto.fit.8)

## There are obvious nonlinearities in the residuals, and there are
## some high leverage points.


##
## 9. Multiple linear regression on Auto
##

## a)
names(Auto)
pairs(Auto)

## b)
cor(Auto[, !(names(Auto) == "name")])

## c)
lm.auto <- lm(
    mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin,
    data=Auto
)
summary(lm.auto)

## i) There is a relationship between the predictors and the response.
## That is, we can reject the null hypothesis that all coefficients
## are equal to zero.
##
## ii) The displacement, weight, year, and origin all have
## statistically significant relationships to the response
##
## iii) The coefficient for year suggests that mpg has been improving
## at a rate of about .75 mpg per year.

## d) There appears to be some non-linearity in the data, but not a lot.

par(mfrow=c(2,2))
plot(lm.auto)

## e)
lm.auto.2 <- lm(mpg ~ displacement + weight + year * origin, data=Auto)
summary(lm.auto.2)

## f) We can increase R^2 from .82 to .85
lm.auto.3 <- lm(mpg ~ displacement + I(displacement^2) + weight + I(weight^2) + year * origin, data=Auto)
summary(lm.auto.3)


##
## 10. Questions about the Carseats dataset
##

## a)
lm.car <- lm(Sales ~ Price + Urban + US, data=Carseats)
summary(lm.car)

## b) Sales are statistically significantly related to price, and
## location in the US.
##
## The base rate of sales (at price zero, outside the US) at a given
## location is 13,000.
##
## Sales decrease by 50 units per dollar increase in price.
##
## Sales increased by 1,200 units if the location was in the united
## states.

## c)
##
## 13.04 - 0.054 * Price - 0.022 * (1 if Urban else 0) + 1.2 (1 if US else 0)

## d) Intercept, Price, USYes

## e)

lm.car.2 <- lm(Sales ~ Price + US, data=Carseats)
summary(lm.car.2)

## f) The R^2 are 0.2393 and 0.2393, so exactly the same. They don't
## fit the model particularly well. The model that removes Urban
## performs equally well to the model with Urban.

## g)
coef(lm.car.2)
confint(lm.car.2, level=0.95)

## h) I see no evidence of outliers or high leverage points
par(mfrow=c(2,2))
plot(lm.car.2)

## 11) Investigating the T-statistice for a linear model without an
## intercept

set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)

plot(x, y)

## a) We have strong evidence that \hat{\beta} =/= 0
lm.no.inter <- lm(y ~ x + 0)
summary(lm.no.inter)

##   Estimate Std. Error t value Pr(>|t|)
## x   1.9939     0.1065   18.73   <2e-16 ***

## b) We have strong evidence that \hat{\beta} =/= 0. In fact, we have
## the same amount of evidence as for (a)
lm.no.inter.2 <- lm(x ~ y + 0)
summary(lm.no.inter.2)

##   Estimate Std. Error t value Pr(>|t|)
## y  0.39111    0.02089   18.73   <2e-16 ***

## c) I can't really tell? THey have the same t-value?

## d) ...
##
## e) ...
##
## f) ...

##
## 12) Simple linear regression without an intercept
##

## a) When the slope is 1? -- Nope...

## b) ...

## c) NOTE: This is wrong
set.seed(2)
x <- rnorm(100)
y <- x + rnorm(100)

summary(lm(x ~ y + 0))
summary(lm(y ~ x + 0))

##
## 13. Simulated data for regression models
##

## a)
set.seed(1)
x <- rnorm(100, sd=1)

## b)
eps <- rnorm(100, sd=sqrt(0.25))

## c)
y <- -1 + 0.5 * x + eps

## y is length 100, \beta_0 = -1, \beta_1 = 0.5.

## d) There is a positive relationship between x & y, passing through
## (0, -1)
plot(x, y)

## e) lm is able to generate, with high confidence, very accurate
## predictions for the parameters
lm.gen <- lm(y ~ x)
summary(lm.gen)
confint(lm.gen)

## f)
plot(x, y)
abline(lm.gen, col="red")
abline(-1, 0.5)
legend(-2, 0, c("predicted", "actual"), c("red", "black"))

## g) It has an RSE of 0.479 whereas the previous model has an RSE of
## 0.4814, so it does explain more of the model.
lm.gen.quad <- lm(y ~ x + I(x ^ 2))
summary(lm.gen.quad)


## h) ...
## i) ...
## j) ...


##
## 14. This problem focuses on the collinearity problem
##

## a)
set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

## y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \varepsilon
##
## \beta_0 = 2
## \beta_1 = 2
## \beta_2 = 0.3

## b)
cor(x1, x2)
plot(x1, x2)

## c)
lm.gen.col <- lm(y ~ x1 + x2)
summary(lm.gen.col)

##             Estimate
## (Intercept)   2.1305
## x1            1.4396
## x2            1.0097
##
## While the estimate of the intercept is close, the rest of these are
## pretty far off.
##
## We can reject the null hypothesis that \beta_1 = 0, but not \beta_2 = 0.


## d)
lm.gen.col.2 <- lm(y ~ x1)
summary(lm.gen.col.2)

## Here, the estimated coefficients are much closer to the true
## values, and it's much easier to reject the null hypothesis.

## e)
lm.gen.col.3 <- lm(y ~ x2)
summary(lm.gen.col.3)

## Here we are very far from the true value of \beta_2, but it is easy
## to reject the null hypothesis.

## f) I'm not sure... Maybe we can talk about this.
## g)

set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm.gen.col.re <- lm(y ~ x1 + x2)
summary(lm.gen.col.re)

## WOW, 101 is a high leverage point!

par(mfrow=c(2,2))
plot(lm.gen.col.re)


##
## 15. Predicting crime rate in the Boston data set
##

## a)
names(Boston)

summary(lm(crim ~ zn, data=Boston)) #
summary(lm(crim ~ indus, data=Boston)) #
summary(lm(crim ~ chas, data=Boston))
summary(lm(crim ~ nox, data=Boston)) #
summary(lm(crim ~ rm, data=Boston)) #
summary(lm(crim ~ age, data=Boston)) #
summary(lm(crim ~ dis, data=Boston)) #
summary(lm(crim ~ rad, data=Boston)) #
summary(lm(crim ~ tax, data=Boston)) #
summary(lm(crim ~ ptratio, data=Boston)) #
summary(lm(crim ~ black, data=Boston)) #
summary(lm(crim ~ lstat, data=Boston)) #
summary(lm(crim ~ medv, data=Boston)) #

## Individually every variable other than adjacency to the charles
## river has a statistically significant effect on crime in isolation.

par(mfrow=c(3, 5))
plot(Boston$crim, Boston$zn)
plot(Boston$crim, Boston$indus)
plot(Boston$crim, Boston$chas)
plot(Boston$crim, Boston$nox)
plot(Boston$crim, Boston$rm)
plot(Boston$crim, Boston$age)
plot(Boston$crim, Boston$dis)
plot(Boston$crim, Boston$rad)
plot(Boston$crim, Boston$tax)
plot(Boston$crim, Boston$ptratio)
plot(Boston$crim, Boston$black)
plot(Boston$crim, Boston$lstat)
plot(Boston$crim, Boston$medv)

## b)
summary(lm(crim ~ ., data=Boston))

## We can reject the null hypothesis for:
##    - zn, dis, rad, black, medv

## c) Very different!

## d) ...
