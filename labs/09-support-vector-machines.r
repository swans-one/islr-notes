library(e1071)  # svm
library(ROCR)   # prediciton, performance

##
## 9.6.1 Support Vecto Classifier
##


## Generate random data, and plot it
set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1,] <- x[y==1,] + 1
plot(x, col=3-y) # col == color
df <- data.frame(x=x, y=as.factor(y)) # data frame with factor "y"

## Fit the data
##
## Note kernel="linear" -> support vector classifier.
svmfit <- svm(y ~ ., data=df, kernel="linear", cost=10, scale=FALSE)

summary(svmfit)
plot(svmfit, df)

names(svmfit)
svmfit$index

## e1071 contains a function to do 10-fold cv `tune`
set.seed(1)
tune.out <- tune(
    svm,
    y ~ .,
    data = df,
    kernel = "linear",
    ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100))
)
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)

## Predictions
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1, 1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdf <- data.frame(x=xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdf)
table(predict = ypred, truth = testdf$y)

##
## 9.6.2 Support Vector Machine
##

## Now we're using a kernel that is not a "linear" kernel

## Create / examine data
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
df2 <- data.frame(x = x, y = as.factor(y))

plot(x, col=y)

# Train a model:
train <- sample(200, 100)
svmfit <- svm(
    y ~ .,
    data = df2[train,],
    kernel = "radial", # Note: now we're doing svm
    gamma = 1,
    cost = 1000 # Playing with this cost is really interesting
)
plot(svmfit, df2[train,])
summary(svmfit)

## Cross validation -- I <3 this
set.seed(1)
tune.out <- tune(
    svm,
    y ~ .,
    data = df2[train,],
    kernel = "radial",
    ranges = list(
        cost = c(0.1, 1, 10, 100, 1000),
        gamma = c(0.5, 1, 2, 3, 4)
    )
)
summary(tune.out)
bestmod2 <- tune.out$best.model

table(
    true = df2[-train, "y"],
    pred = predict(bestmod2, newdata = df2[-train,])
)

##
## 9.6.3 ROC Curves
##
rocplot <- function(pred, truth, ...) {
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}


svmfit.opt <- svm(
    y ~ .,
    data = df2[train, ],
    kernel = "radial",
    gamma = 2,
    cost = 1,
    decision.values = T
)
fitted <- attributes(
    predict(
        svmfit.opt,
        df2[train,],
        decision.values=TRUE
    )
)$decision.values

par(mfrow=c(1,2))
rocplot(fitted, df2[train, "y"], main="Training Data")
