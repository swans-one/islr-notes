## 10. Use the Weekly data set. Similar to Smarket, except it contains
## 1,089 weekly returns for 21 years, 1990-2010.

library(ISLR)

## a) Produce some numerical and graphical summaries of Weekly data.

summary(Weekly)
plot(Weekly)

plot(Weekly$Volume)


## b) Use the full data set ot perform a logistic regression with
## Direction as the response and the five lag variables plus Volume as
## predictors.

glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Weekly,
    family = binomial)

summary(glm.fits)


## c) Compute the confusion matrix

preds <- predict(glm.fits, Weekly, type='response')

act.dir <- Weekly$Direction
pred.dir <- rep("Down", length(preds))
pred.dir[preds > 0.5] <- "Up"
table(pred.dir, act.dir)

## ANSWER: Basically, this is always predicting "Up", which is weird.

## d) Fit logistic regression using a training data period from 1990
## to 2008, with lag2 as the only predictor.

train <- Weekly$Year < 2009
test <- Weekly$Year >= 2009

glm.fits.train <- glm(
    Direction ~ Lag2,
    data = Weekly,
    subset = train,
    family = binomial)

preds.test <- predict(glm.fits.train, Weekly[test,])
pred.test.dir <- rep("Down", length(preds.test))
pred.test.dir[preds.test > 0.5] <- "Up"

table(pred.test.dir, Weekly[test, "Direction"])
mean(pred.test.dir == Weekly[test, "Direction"])

## e)

## 12. This problem involves writing functions.

## a) Write a function, Power(), that prints out the result of raising
## 2 to the 3rd power.

Power <- function () {
    print(2^3)
}
Power()

## b) Createa a new function Power2() that allows you to pass any two
## numbers, x, and a and prints the value of x^a.

Power2 <- function (x, a) {
    print(x^a)
}
Power2(2, 3)

## c) Using the Power2 function you just wrote, compute 10^3, 8^17,
## 131^3

Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

## d) Now createa a new function that actually, returns the result as
## an R object.

Power3 <- function (x, a) {
    return(x^a)
}

## e) using the Power3 function, createa a plot of $f(x) = x^2$
plot(1:10, Power3(1:10, 2), xlab = "x", ylab = "x^2 (log-scale)", log = "y")

## f) Create a function PlotPower() that allows you to createa a plot
## of x against x^a for a fix a, and a range of values x.

PlotPower <- function (r, a) {
    plot(r, Power3(r, a), xlab = "x", ylab = "x^2")
}

PlotPower(1:10, 3)
