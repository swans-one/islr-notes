library(ISLR)
library(MASS)
library(class)

## Percentage returs for the S&P 500 over 1,250 days, 2001 to 2005.
names(Smarket)
dim(Smarket)
summary(Smarket)

cor(Smarket[,-9])

## The only substantial correlation we see is volume increasing over
## time.
plot(Smarket$Volume)

##
## 4.6.2 Logistic Regression
############################

## GLM == Generalized linear models
##
## We use `family=binomial` to specify logistic regression
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket,
    family = binomial,
)
summary(glm.fits)
coef(glm.fits)

## Predict will use the training data if no extra data is provided.
glm.probs <- predict(glm.fits, type="response")
glm.probs[1:10]

## This function shows us the direction
contrasts(Smarket$Direction)

## We can convert these probabilities into predictions
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] <- "Up"

## And then make a confusion matrix
table(glm.pred, Smarket$Direction)


## Creating a test set
train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)

## We can now fit our model using the `subset` argument to subset our
## data
glm.fits.train <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket,
    family = binomial,
    subset = train,
)
summary(glm.fits.train)

glm.probs.test <- predict(glm.fits.train, Smarket.2005, type="response")
glm.pred.test <- rep("Down", 252)
glm.pred.test[glm.probs.test > 0.5] <- "Up"

## Now our confusion matrix will not reflect training error, but test
## error:
table(glm.pred.test, Smarket.2005$Direction)
mean(glm.pred.test != Smarket.2005$Direction) # The error rate

## Trying again with fewer predictors
glm.fits.train2 <- glm(
    Direction ~ Lag1 + Lag2,
    data = Smarket,
    family = binomial,
    subset = train,
)
summary(glm.fits.train2)
glm.probs.test2 <- predict(glm.fits.train2, Smarket.2005, type="response")
glm.pred.test2 <- rep("Down", 252)
glm.pred.test2[glm.probs.test2 > 0.5] <- "Up"
table(glm.pred.test2, Smarket.2005$Direction)
mean(glm.pred.test2 != Smarket.2005$Direction) # The error rate

predict(
    glm.fits.train2,
    newdata = data.frame(
        Lag1 = c(1.2, 1.5),
        Lag2 = c(1.1, -0.8)
        ),
    type = "response"
)


##
## 4.6.3 Linear Discriminant Analysis
#####################################

## The `lda` function is part of the `MASS` library
lda.fit <- lda(
    Direction ~ Lag1 + Lag2,
    data=Smarket,
    subset=train
)
lda.fit
summary(lda.fit)
plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Smarket.2005$Direction)
mean(lda.class==Smarket.2005$Direction)


##
## 4.6.4 Quadratic Discriminant Analysis
########################################


qda.fit <- qda(
    Direction ~ Lag1 + Lag2,
    data=Smarket,
    subset=train
)
qda.fit
summary(qda.fit)

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Smarket.2005$Direction)
mean(qda.class == Smarket.2005$Direction)

##
## 4.6.5 K-Nearest Neighbors
############################

## The `knn` function is part of the `class` library

train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[!train,]
Direction.2005 <- Smarket.2005$Direction

train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train,]
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
train.Direction <- Smarket$Direction[train]

## KNN with k=1
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
length(knn.pred)
mean(knn.pred == Direction.2005)
table(knn.pred, Direction.2005)

## KNN with k=3
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
length(knn.pred)
mean(knn.pred == Direction.2005)
table(knn.pred, Direction.2005)


##
## 4.6.6 An application to Caravan Insurance Data
#################################################


## The `Caravan` dataset is part of the `ISLR` library.
dim(Caravan)

## We want to `standardize` the data
standardized.X <- scale(Caravan[,-86])
c(var(Caravan[,1]), var(standardized.X[,1]))

test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Caravan$Purchase[-test]
test.Y <- Caravan$Purchase[test]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
