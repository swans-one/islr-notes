library(MASS)
library(ISLR)

##
## 3.6.2 Simple Linear Regression
##

## The Boston dataset contains information about 506 neighborhoods in
## Boston. We're interested in median house value: medv
fix(Boston)
names(Boston)

?Boston

## Fit a model
lm.fit <- lm(medv ~ lstat, data=Boston)

## See a bunch of information about it
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
confint(lm.fit)

## Make predictions based on the model
predict(lm.fit) # Predict on the training data
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval="confidence")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval="prediction")

## Plot the data, and add the fit line to the plot
plot(Boston$lstat, Boston$medv)
abline(lm.fit, lwd=3, col="red")

## We can get several diagnostics plot simply by calling `plot` on the
## output of `lm`.
par(mfrow=c(2,2))
plot(lm.fit)

## We can also plot the residuals versus predictions ourselves
par(mfrow=c(1,1))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

## leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

##
## 3.6.3 Multiple Linear Regression
##

## Fit more than one term
lm.fit <- lm(medv ~ lstat + age, data=Boston)
summary(lm.fit)

## Or fit all the terms in the data frame
lm.fit <- lm(medv ~ ., data=Boston)
summary(lm.fit)

# The lm summary elements can be accessed individually
?summary.lm

summary(lm.fit)$r.sq
summary(lm.fit)$sigma

##
## 3.6.4 Interaction Terms
##

## To add just a single interaction, use `lstat:black`
##
## To add the interaction and the terms themselves use `lstat*black`

summary(lm(medv ~ lstat * age, data = Boston))

##
## 3.6.5 Non-linear Transformations of the Predictors
##

## We can perform transformations on the terms of the parameters by
## wrapping the transformation in the `I` function
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data=Boston)
summary(lm.fit2)

## We can see that these models are significantly different
lm.fit <- lm(medv ~ lstat, data=Boston)
anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

## If we want to add higher order polynomials we can use `poly`
lm.fit5 <- lm(medv ~ poly(lstat, 5), data=Boston)
summary(lm.fit5)

## We can do other transformations too
summary(lm(medv ~ log(rm), data=Boston))

##
## 3.6.6 Qualitative Predictors
##

## We'll use the Carseats data, which is part of ISLR

fix(Carseats)
names(Carseats)

## R automatically generates dummy variables from indicator variables
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data=Carseats)
summary(lm.fit)

##
## 3.6.7 Writing Functions
##

LoadLibraries <- function() {
    library(ISLR)
    library(MASS)
    print("The libraries have been loaded")
}
