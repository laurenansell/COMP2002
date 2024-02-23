### Load in the libraries (for the method specific ones I'll add as I go)
library(tidyverse)

### For each of the methods I will use either the iris data or the Longley's Economic
### Regression data as they are built in datasets.

attach(iris)
attach(longley)

## PCA

iris_pca <- princomp(iris[, -5], cor = TRUE) 

## knn
library(class)

## the package class has a knn function which uses leave-one-out cross validation called
## knn.cv

iris[,1:4] <- scale(iris[,1:4])
setosa<- rbind(iris[iris$Species=="setosa",])
versicolor<- rbind(iris[iris$Species=="versicolor",])
virginica<- rbind(iris[iris$Species=="virginica",])


ind <- sample(1:nrow(setosa), nrow(setosa)*0.8)
iris.train<- rbind(setosa[ind,], versicolor[ind,], virginica[ind,])
iris.test<- rbind(setosa[-ind,], versicolor[-ind,], virginica[-ind,])
iris[,1:4] <- scale(iris[,1:4])

knn.fit <- knn.cv(train = iris.train[,1:4], cl = iris.train$Species, k = 4)


