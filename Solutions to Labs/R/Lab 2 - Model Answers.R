library(tidyverse)
library(ISLR)
library(class)
library(cluster) 
library(factoextra)
library(keras)


## Solution to exercise 2

## The iris dataset is also automatically included as one of the standard datasets in R. To bring in into our
## environment, we use the attach() function.

attach(iris)

## Now we can access all of the columns of data contained within the dataset by simply writing the column name, e.g.

head(Sepal.Length) ## Gives the first 6 entries in the column.

## Now we perform PCA on the first 4 column as the final column contains the class label. The -5 within the [] brackets
## tells R to drop only the 5th column of the dataset.

iris_pca <- princomp(iris[, -5], cor = TRUE) 

## Now we store the results of the PCA in a new dataframe
new_variables <- as.data.frame(iris_pca$scores)

## And add the class label information
new_variables$species <- iris_data$Species

## We only want to keep the information about the first and second principal components:

new_variables<-new_variables[,c(1,2,5)]


## Now fit the KNN classifier

knn.fit <- knn.cv(train = new_variables[,1:2], cl = new_variables$species, k = 10)

## Add these to the dataframe

new_variables$species_fitted<-knn.fit

## Now plot the results

ggplot(new_variables, aes(x = Comp.1, y = Comp.2,col=species_fitted)) + geom_point(size=3)+
  labs(x = "First Principal Component", 
       y = "Second Principal Component") +
  coord_fixed(ratio = 1)+
  theme(axis.title = element_text(size=16),
        legend.position = "none")


## Extension task - Using a loop to find the optimal K value

## First we need an empty vector to store the error values
error <- c()

## Now we write the loop:
for (i in 1:25){
  knn.fit <- knn.cv(train = new_variables[,1:2], cl = new_variables$species, k = i) ## Fit the classifier
  error[i] = 1- mean(knn.fit == Species) # Stores the error
}


## Plot on a scatter plot

ggplot(data = data.frame(error), aes(x = 1:25, y = error)) +
  geom_line(color = "Blue",linewidth=2)+labs(x="Number of clusters")+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

## Solution to exercise 3

## To read in external data we use the read.csv() function. 

iris_data<-read.csv("Iris.csv") ## To use this the file MUST BE in saved in the same location as the R script.

iris_data<-read.csv("C://Users/your_file_path/Iris.csv") ## You can also use the whole file path

## Once the data is read in, from here the code is the same as exercise 2.

## Solution to exercise 4

## The MNIST dataset can be read in through the keras package

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y

# visualize the digits
par(mfcol=c(6,6))
par(mar=c(0, 0, 3, 0), xaxs='i', yaxs='i')
for (idx in 1:36) { 
  im <- x_train[idx,,]
  im <- t(apply(im, 2, rev)) 
  image(1:28, 1:28, im, col=gray((0:255)/255), 
        xaxt='n', main=paste(y_train[idx]))
}


## To do the classification, I have uploaded the csv files of this dataset which are available from Kaggle:
## https://www.kaggle.com/datasets/oddrationale/mnist-in-csv

mnist<-read.csv("mnist_train.csv")

mnist_pca <- princomp(mnist[, -1]) 


## Now we store the results of the PCA in a new dataframe
new_variables <- as.data.frame(mnist_pca$scores)

## And add the class label information
new_variables$labels <- mnist$label

## We only want to keep the information about the first and second principal components:

new_variables<-new_variables[,c(1,2,785)]


## Now plot the results

ggplot(new_variables, aes(x = Comp.1, y = Comp.2,col=labels)) + geom_point(size=3)+
  labs(x = "First Principal Component", 
       y = "Second Principal Component") +
  coord_fixed(ratio = 1)+
  theme(axis.title = element_text(size=16),
        legend.position = "right")
