library(ISLR)

## 5. We'll estimate the test error of a logistic regression model on
## the `Default` dataset using the validation set approach.

## a) Fit a logistic regression model that usese income and balance to
## predict default

glm.fit <- glm(default ~ income + balance, data=Default, family="binomial")
summary(glm.fit)

## b) Using the validation set approach estimate the test error of
## this model

run.experiment <- function(seed) {
    set.seed(seed)
    n <- dim(Default)[1]
    train <- sample(n, 2 * n/3)

    glm.train <- glm(default ~ income + balance, data=Default, family="binomial", subset=train)

    pred <- rep("No", n - length(train))
    glm.pred <- predict(glm.train, Default[-train,], type="response")
    pred[glm.pred > 0.5] <- "Yes"

    err <- sum(Default$default[-train] != pred) / length(pred)
    print(err)
}

## c) Repeat b three times, comment on the results obtained

run.experiment(1)
run.experiment(2)
run.experiment(3)

## d) Now do the same, but include the dummy variable for `student`
## and comment if it changes the error rate.


set.seed(1)
n <- dim(Default)[1]
train <- sample(n, 2 * n/3)

glm.train <- glm(default ~ income + balance + student, data=Default, family="binomial", subset=train)

pred <- rep("No", n - length(train))
glm.pred <- predict(glm.train, Default[-train,], type="response")
pred[glm.pred > 0.5] <- "Yes"

err <- sum(Default$default[-train] != pred) / length(pred)
print(err)

## It doesn't change the error rate.

## 6. Use the bootstrap and standard glm formula to estimate errors

## a) Using the summary and glm functions, determine the estimated
## standard errors for the coefficients associated with income and
## balance.
glm.fit <- glm(default ~ income + balance, data=Default, family="binomial")
summary(glm.fit)

## b) Write a function, boot.fn() that takes as input the Default data
## set as well as an index of the observations and that outputs the
## coefficient estimates for income and balance in the multiple
## logistic regression model.

boot.fn <- function(data, index) {
    model <- glm(default ~ income + balance, data=data, family="binomial", subset=index)
    model$coef[c("income", "balance")]
}

## c) Use the boot() function together with your boot.fn() function to
## estimate the standard errors of the logistic regression
## coefficients for income and balance.

boot(Default, boot.fn, 100)

## 7. Let's use a Loop to compute LOOCV

## a) Fit a logistic regression model that predicts Direction using
## Lag1 and Lag2
glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial)
summary(glm.fit)

## b) Fit a logistic regression model that predicts Direction using
## Lag1 and Lag2
glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Weekly[-1,], family=binomial)

## c) Use the model from (b) to predict the direction of the first
## observation, are we right?
predict(glm.fit, Weekly[1,], type="response") > 0.5
Weekly$Direction[1]

## No! We did not predict correctly

## d) Write a for loop from i = 1 to i = n, where n is the number of
## observations in the dataset.

n <- dim(Weekly)[1]
results <- rep(NA, n)
for (i in 1:n) {
    fit.model <- glm(Direction ~ Lag1 + Lag2, data=Weekly[-i,], family=binomial)
    prob <- predict(fit.model, Weekly[i,], type="response")
    up.pred <- prob > 0.5
    up.actual <- Weekly$Direction[i] == "Up"
    results[i] <- up.pred != up.actual
}
results

## e) Take the average of the n numbers attained in (d) to obtain the
## LOOCV estimate for the test error, comment on the result

mean(results)

## A test error of 0.45 actually isn't that bad considering we're
## predicting stock market data.

## 8. We will now perform cross-validation on a simulated data set

## a) Create the dataset:

set.seed(1)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)
df <- data.frame(x=x, y=y)

## b) Create a scatterplot

plot(df$x, df$y)

## An inverted-U relationship, with fewer examples at the tails

## c) Set a random seed and then compute the LOOCY errors that result
## from fitting the following models, using least squares

loocv <- function(model, data, seed, debug=FALSE) {
   set.seed(seed)
    glm.fit <- glm(model, data=data)
    if (debug) {
        print(summary(glm.fit))
    }
    cv.err <- cv.glm(data=data, glm.fit)
    cv.err$delta[1]
}

loocv(y ~ x, df, 1)
loocv(y ~ x +  I(x^2), df, 1)
loocv(y ~ x + I(x^2) + I(x^3), df, 1)
loocv(y ~ x + I(x^2) + I(x^3) + I(x^4), df, 1)

## d) Repeat (c) with a different seed. Are the results the same?

loocv(y ~ x, df, 123)
loocv(y ~ x +  I(x^2), df, 123)
loocv(y ~ x + I(x^2) + I(x^3), df, 123)
loocv(y ~ x + I(x^2) + I(x^3) + I(x^4), df, 123)

## They're exactly the same, because there is no randomness in LOOCV

## e) Which of the models in (c) has the smallest LOOCV error. Is it
## what you expected?

## The model with the highest degree polynomial has the lowest LOOCV
## error. This is not necessarily what you would expect, since a
## higher degree of polynomial could leave you prone to
## overfitting. However, the error value is not so much lower than the
## 2nd-degree polynomial that you would prefer the higher degree
## model.

## f) Comment on the statistical significance of the coefficient
## estimates that result from fitting each model in (c)

loocv(y ~ x, df, 1, debug=TRUE)
loocv(y ~ x +  I(x^2), df, 1, debug=TRUE)
loocv(y ~ x + I(x^2) + I(x^3), df, 1, debug=TRUE)
loocv(y ~ x + I(x^2) + I(x^3) + I(x^4), df, 1, debug=TRUE)

## The third and fourth degree polynomial terms are never signficant,
## which is to be expected. The first degree term is only mildly
## significant if you don't include the second degree term.
