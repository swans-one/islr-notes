x <- c(1, 3, 2, 5)
y = c(1, 4, 3)

length(x)
length(y)

x + y

## See help
?matrix

x2 <- matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x2

## Assign by rows
x3 <- matrix(data=c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
x3

sqrt(x3)

## vector of (reproducible) random normals
set.seed(1303)
x <- rnorm(50)
y <- x + rnorm(50, mean=50, sd=0.1)
cor(x, y)
mean(y)
var(y)
sqrt(var(y))
sd(y)


## plotting
x <- rnorm(100)
y <- rnorm(100)
# plot(x, y)
plot(x, y, xlab="this is the x-axis", ylab="this is the y-axis", main="Plot of X vs Y")

## Saving a plot
pdf("Figure.pdf")
plot(x, y, col="green")
dev.off()


## Sequences of numbers
x <- seq(1, 10)
x
1:10
seq(-pi, pi, length=50)


## Three dimensional plots
##########################

## contour plots
y <- x
f <- outer(x,y, function(x,y) cos(y) / (1+x^2))
## contour(x, y, f)
contour(x, y, f, nlevels=45, add=T)
fa <- (f-t(f))/2
contour(x, y, fa, nlevels=15)

## heatmaps
image(x, y, fa)

## Perspective plots
## persp(x, y, fa)
persp(x, y, fa, theta=30, phi=40)


## Indexing Data
################

A <- matrix(1:16, 4, 4)
A
A[2,3]
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2,]
A[,1:2]
## exclude rows/columns
A[-c(1,3),]
dim(A)


## Loading Data
###############

Auto <- read.table("./data/Auto.data")

## Look at the data in a tabular format
## fix(Auto)

## Use headers, treat ? as na
Auto <- read.table("./data/Auto.data", header=T, na.strings="?")

dim(Auto)
Auto[1:4,]

dim(na.omit(Auto))
names(Auto)


## More Plotting and summary methods
####################################

plot(Auto$cylinders, Auto$mpg)

## We can attach a dataset to make this easier
## attach(Auto)
## plot(cylinders, mpg)

Auto$cylinders=as.factor(Auto$cylinders)
plot(Auto$cylinders, Auto$mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

## histograms
hist(Auto$mpg)
hist(Auto$mpg, col=2, breaks=15)

## Scatterplots for every pair of data
pairs(Auto)
## And a selection of the pairs
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

## Identify lets us see values at a point
plot(Auto$horsepower, Auto$mpg)
identify(Auto$horsepower, Auto$mpg, Auto$name)

## Summarize every column in a dataset
summary(Auto)
