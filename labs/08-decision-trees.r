library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)

##
## Classification Trees
##

## Analyzing the `Carseats` data set (from ISLR)

## Anaysis
dim(Carseats)
head(Carseats)
summary(Carseats)

## Feature cleaning
High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

## Train & eval the decision tree
tree.carseats <- tree(High ~ . - Sales, Carseats)
tree.carseats
summary(tree.carseats)

## Plot the decison tree
plot(tree.carseats)
text(tree.carseats, pretty=0)


## Evaluating performance with CV

## setup test set CV
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]

## Training / Testing
tree.carseats.2 <- tree(High ~ . -Sales, Carseats, subset=train)
tree.pred <- predict(tree.carseats.2, Carseats.test, type="class")
table(tree.pred, High.test)


## Pruning

## Setup / pruning eval
set.seed(3)
cv.carseats <- cv.tree(tree.carseats.2, FUN=prune.misclass)
names(cv.carseats)
cv.carseats # note, lowest cv error rate (dev) at size 9

## Pruning
prune.carseats <- prune.misclass(tree.carseats, best=9)

## Plotting
plot(prune.carseats)
text(prune.carseats, pretty=0)

## CV Eval
pruned.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)

##
## Regression Trees
##

## Boston dataset (from MASS)

## setup
set.seed(1)
bos.train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~., Boston, subset=train)
summary(tree.boston)

## plotting
plot(tree.boston)
text(tree.boston, pretty=0)

## CV / pruning
cv.boston <- cv.tree(tree.boston)
cv.boston # note that pruning doesn't reduce deviance

## Test-set eval
yhat <- predict(tree.boston, newdata=Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2)


##
## Bagging and Random Forests
##

## Using the Boston dataset (from MASS) and the randomForest package

## Summary
head(Boston)
dim(Boston)


## Bagging
## Note: mtry=13 means: use 13 predictors (all of them) == Bagging
set.seed(1)
bag.boston <- randomForest(medv ~ ., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston

## Bagging Eval
## wow, this looks a lot better than a single tree
yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

## MSE of 13.4 much better than 26.3 for a single tree


## Random forest
## we reduce the number of mtry to create a random forest
set.seed(1)
rf.boston <- randomForest(medv ~ ., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf - boston.test)^2)
## MSE of 13.0 is slightly better than bagging

## Importance
## Since we specified `importance=TRUE` in the training command, we
## can use the importance function.
importance(rf.boston)


##
## Boosting
##

## Using the boston dataset and the `gbm` function (from gbm)

## Training
##
## Notes:
##   - use distribution="gaussian" for regression ("bernoulli" for classification)
set.seed(1)
boost.boston <- gbm(
    medv ~ .,
    data=Boston[train,],
    distribution="gaussian",
    n.trees=5000,
    interaction.depth=4
)
summary(boost.boston)

## Plotting
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)
## MSE: 18.5 -- not better than random forests
