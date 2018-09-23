library(ISLR)
library(splines)
library(gam)

## 7.8.1 Polynomial Regression and Step Functions

## Three equivalent ways to do polynomial fitting
fit1 <- lm(wage ~ poly(age, 4), data=Wage)
fit2 <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)
fit3 <- lm(wage ~ cbind(age, age^2, age^3, age^4), data=Wage)
summary(fit)
summary(fit2)
summary(fit3)

agelims <- range(Wage$age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit1, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit )

par(mfrow=c(1,1), mar=c(4.5, 4.5, 1, 1), oma=c(0,0,4,0))
plot(Wage$age, Wage$wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

preds2 <- predict(fit2, newdatat=list(age=age.grid), se=TRUE)

## We can do logistic regression too
fit <- glm(I(wage > 250) ~ poly(age, 4), data=Wage, family=binomial)
preds <- predict(fit, newdata=list(age=age.grid), type="response", se=T)


## 7.8.2 Splines

## splines with specific knots

fit <- lm(wage ~ bs(age, knots=c(25, 40, 60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=T)
plot(Wage$age, Wage$wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2*pred$se, lty="dashed")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed")

## Smoothing splines

plot(Wage$age, Wage$wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Smoothing Spline")
fit <- somoth.spline(Wage$age, Wage$wage, df=16)
fit2 <-smooth.spline(Wage$age, Wage$wage, cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"),
       col=c("red", "blue"), lty=1, lwd=2, cex=0.8)

## 7.8.3 GAMs

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data=Wage)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data=Wage)

par(mfrow=c(1, 3))
plot(gam.m3, se=TRUE, col="blue")

plot.Gam(gam1, se=TRUE, col="red")


## In the GAM plot, year looks pretty linear, we can use anova to
## check

gam.m1 <- gam(wage ~ s(age, 5) + education, data=Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F")

## Note in the summary that the anova for non-parametric effects is
## testing against a linear term
summary(gam.m3)


## Gams can also be used in logistic regression models. Here we
## predict high earners (and exclude < HS Grad because there were no
## high earners in that category).
table(Wage$education, I(Wage$wage > 250))

gam.lr.s <- gam(
    I(wage > 250) ~ year + s(age, df=5) + education,
    data=Wage,
    family=binomial,
    subset=(education != "1. < HS Grad")
)
plot(gam.lr.s, se=T, col="green")
