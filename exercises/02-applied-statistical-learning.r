## 8. This exercise relates to the College data set, which can be
## found in the file College.csv. It contains a number of variables
## for 777 different universities and college in the US.
##
## a) Use the read.csv() function to read the data into R. Call the
## loaded data college.

college <- read.csv('../labs/data/College.csv')

## b)

rownames(college) <- college[,1]
college <- college[,-1]

## c)

summary(college) # i
pairs(college[,1:10]) # ii
plot(
    college$Private,
    college$Outstate,
    xlab="Private School?",
    ylab="Out of state tuition"
) # iii

## iv
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college$Elite <- Elite

## v
par(mfrow = c(2, 2))
hist(college$Apps, xlim=c(0,20000), breaks=100)
hist(college$Room.Board)
hist(college$S.F.Ratio, breaks=20)
hist(college$PhD)


## 9. This exercise involves the Auto data set studied in the
## lab. Make sure that the missing values have been removed from teh
## data.

auto <- read.table("../labs/data/Auto.data", header=TRUE, na.strings="?")
auto <- na.omit(auto)

## a)
##    qualitative: year, origin, name
##    quantitative: mpg, displacement, horsepower, weight, acceleration
##    Maybe either: cylinders

## b)

summary(auto)

##    mpg:          [9.0, 46.6]
##    displacement: [68.0, 455.0]
##    horsepower:   [46, 230]
##    weight:       [1613, 5140]
##    acceleration: [8.0, 24.8]

## c)

##    mpg          -- mean: 23.5  sd: 7.8
##    displacement -- mean: 193.5 sd: 104.6
##    horsepower   -- mean: 104.5 sd: 38.5
##    weight       -- mean: 2970  sd: 849.4
##    acceleration -- mean: 15.6  sd: 2.8

## d)

summary(auto[-(10:85),])

## e)

## Todo

## f)

par(mfrow = c(2, 2))
plot(as.factor(auto$cylinders), auto$mpg)
plot(auto$weight, auto$mpg)
plot(auto$year, auto$mpg)
plot(auto$horsepower, auto$mpg)

## Yeah! MPG looks related to cylinders, weight, and year

## 10. This exercise involves the Boston housing data set.

## a)

library(MASS)
dim(Boston)

## - ‘crim’ per capita crime rate by town.
## - ‘zn’ proportion of residential land zoned for lots over 25,000 sq.ft.
## - ‘indus’ proportion of non-retail business acres per town.
## - ‘chas’ Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
## - ‘nox’ nitrogen oxides concentration (parts per 10 million).
## - ‘rm’ average number of rooms per dwelling.
## - ‘age’ proportion of owner-occupied units built prior to 1940.
## - ‘dis’ weighted mean of distances to five Boston employment centres.
## - ‘rad’ index of accessibility to radial highways.
## - ‘tax’ full-value property-tax rate per \$10,000.
## - ‘ptratio’ pupil-teacher ratio by town.
## - ‘black’ 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
## - ‘lstat’ lower status of the population (percent).
## - ‘medv’ median value of owner-occupied homes in \$1000s.

## b)

pairs(~ crim + zn + indus + tax, data=Boston)

## c)

cor(Boston)[1,]
 ##       crim          zn       indus        chas         nox          rm
 ## 1.00000000 -0.20046922  0.40658341 -0.05589158  0.42097171 -0.21924670
 ##        age         dis         rad         tax     ptratio       black
 ## 0.35273425 -0.37967009  0.62550515  0.58276431  0.28994558 -0.38506394
 ##      lstat        medv
 ## 0.45562148 -0.38830461

## The strongest correlations are
##   - rad: index of accessibility to radial highways.
##   - tax: full-value property-tax rate per \$10,000.
##   - lstat: lower status of the population (percent).

## d)

summary(Boston)

## - Crime rate max: 88.98 (75% == 3.7)
## - Tax rate max: 771 (75% == 666)
## - Pupil teacher ratio max: 22 (75% == 20.2)

## e)

sum(Boston$chas) # 35

## f)

summary(Boston$ptratio)
## 19.05

## g)

which.min(Boston$medv) # 399
Boston[399,]

##        crim zn indus chas   nox    rm age    dis rad tax ptratio black lstat medv
## 399 38.3518  0  18.1    0 0.693 5.453 100 1.4896  24 666    20.2 396.9 30.59    5

## - Much higher crime than average
## - No land zoned for large lots
## - 75% for industrial usage
## - Not near the charles river
## - Higher than 75% for nox
## - Below 25% for rooms / dwelling
## - All old buildings
## - Very close to the employment centers
## - Very accessible by highways
## - High tax rate per 10k
## - Higher than average pupile teacher ratio
## - Largest black population
## - High proportion of lstat

## h)

sum(Boston$rm > 7) # 64
sum(Boston$rm > 8) # 13
sum(Boston$rm > 9) # 0

summary(Boston[Boston$rm > 8])
