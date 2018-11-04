library(ISLR)

##
## 10.4 Lab 1: Principal Components Analysis
##

## We'll be using the USArrests dataset

states <- row.names(USArrests)

## Each measure's mean, variance
apply(USArrests, 2, mean)
apply(USArrests, 2, var)


## PCA with prcomp
pr.out <- prcomp(USArrests, scale=TRUE)

## The center & scale -> mean & sd
pr.out$center
pr.out$scale

## The `rotation` is the pca vector loadings
pr.out$rotation

## Plot the first two principal components
biplot(pr.out, scale=0)

## Compute the variance explained by each principal component
pr.out$sdev
pr.var <- pr.out$sdev ^ 2
percent.var.explained <- pr.var / sum(pr.var)
percent.var.explained # The first PC explains 62% of variance

##
## 10.5 Lab 2: Clustering
##

##
## 10.5.1 K-means clustering
##

## K-means with the `kmeans` function

## Generate some data
set.seed(2)
x <- matrix(rnorm(50*2), ncol=2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

## Plot raw data
plot(x)

## Perform clustering
## 2 clusters (k=2)
km.out <- kmeans(x, 2, nstart=20)
names(km.out)
km.out

## Plot results
plot(x, col=(km.out$cluster+1), pch=19)


## Perform clustering
## 3 clusters (k=2)
km.out.3 <- kmeans(x, 3, nstart=20)
names(km.out.3)
km.out.3

## Plot results
plot(x, col=(km.out.3$cluster+1), pch=19)


## Using nstart=1 to only do one random assignment
km.out.rand <- kmeans(x, 3, nstart=1)
names(km.out.rand)
km.out.rand
plot(x, col=(km.out.rand$cluster+1), pch=19)

##
## 10.5.2 Hierarchical Clustering
##

## We use `hclust` to perform hierarchical clustering

## Using the different linkage methods
hc.centroid <- hclust(dist(x), method="centroid")
hc.single   <- hclust(dist(x), method="single")
hc.average  <- hclust(dist(x), method="average")
hc.complete <- hclust(dist(x), method="complete")

## Plot a comparison
par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage")
plot(hc.average, main="Average Linkage")
plot(hc.single, main="Single Linkage")

## Plot a side-by side
set.seed(2)
x2 <- matrix(rnorm(26*2), ncol=2)
x2[1:13, 1] <- x2[1:13, 1] + 3
x2[1:13, 2] <- x2[1:13, 2] - 4
x2.hc.complete <- hclust(dist(x2), method="complete")
par(mfrow=c(1,2))
plot(x2, cex=0)
text(x2, labels=c(1:26), cex=0.7)
plot(x2.hc.complete)

## Determine cluster labels with `cutree`
cutree(hc.complete, 2)

##
## 10.6 Lab 3: NCI60 Data Example
##

## Using a genomics dataset from ISLR
##
## 64 rows, 6830 columns, columns represent gene expression levels
nci.labs <- NCI60$labs
nci.data <- NCI60$data

##
## 10.6.1 PCA on NCI60
##

## Perform pca
pr.nci <- prcomp(nci.data, scale=TRUE)

## Assign a unique color to every unique element in a vector:
rainbow.col <- function(vec) {
    cols <- rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}

## Plot the first few score vectors
##
## Color by cancer cell line
par(mfrow=c(1,2))
plot(
    pr.nci$x[,1:2],
    xlab="Z1", ylab="Z2",
    col=rainbow.col(nci.labs),
    pch=19,
)
plot(
    pr.nci$x[,c(1,3)],
    xlab="Z1", ylab="Z2",
    col=rainbow.col(nci.labs),
    pch=19,
)

## We can get a summary of the proportion of variance explained (PVE)
## of the first few principal components
summary(pr.nci)

## And we can plot this in a scree plot
pve <- 100 * (pr.nci$sdev ^ 2) / sum(pr.nci$sdev ^ 2)
par(mfrow=c(2,1))
plot(pve)
plot(cumsum(pve), type="o")

##
## 10.6.2 Clustering the observations of the NCI 60 Data
##

## Start by scaling the data
scaled.nci <- scale(nci.data)
dist.nci <- dist(scaled.nci)

nci.single   <- hclust(dist.nci, method="single")
nci.average  <- hclust(dist.nci, method="average")
nci.complete <- hclust(dist.nci, method="complete")

par(mfrow=c(2,1))
plot(nci.complete, labels=nci.labs, main="Complete", xlab="", sub="")
## plot(nci.average, labels=nci.labs, main="Average", xlab="", sub="")
plot(nci.single, labels=nci.labs, main="Single", xlab="", sub="")
