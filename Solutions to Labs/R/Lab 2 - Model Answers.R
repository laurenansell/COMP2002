library(tidyverse)
library(ISLR)
library(class)
library(cluster) 
library(factoextra)
library(keras)
library(mlbench)
library(tree) 

## Solution to exercise 2

## For this first exercise we will be using a decision tree to classify the 
## breast cancer dataset.

data("BreastCancer") ## Brings the dataset into the environment

head(BreastCancer) ## Check it has imported correctly

def.subset <- sample(699, 559) ## create a list of indices to create the training set    

data_training<-BreastCancer[def.subset,]

data_test<-BreastCancer[-def.subset,]

test_labels<-BreastCancer$Class[-def.subset]

## Now we are ready to create the tree on the training data.

tree.breastcancer = tree(Class ~ . - Class, data = data_training)

## If we wish we can plot the tree using base R:
plot(tree.breastcancer)
text(tree.breastcancer,pretty = 0) ## adds the labels


## As trees are prone to overfitting, we want to find the missclassification
## rate for the training data.

training_error<-summary(tree.breastcancer)

training_error<-training_error$misclass

training_error<-training_error[1]/training_error[2]

tree.pred = predict(tree.breastcancer, data_test, type = "class") 

# Make predictions for the test data using the tree.

tab <- table(tree.pred, test_labels) 

test_error<-(tab[1,2] + tab[2,1]) / sum(tab)  # test error

## Extension task - pruning the tree

## Our tree had 11 terminal nodes, the prune.tree() function will systemically
## move through the tree and removing (pruning) the least important parts.
prune.tree(tree.breastcancer,method = "misclass")


prune_result<-prune.tree(tree.breastcancer,method = "misclass")

## Lets look at the deviance, this is the number of misclassifications in each 
## sub tree

plot(prune_result$dev)

## Looking at the plot the we see the number of misclassified data points increasing
## as the number of terminal nodes is decreased.

## Using ggplot

best_tree<-as.data.frame(cbind(prune_result$size,prune_result$dev))

ggplot(best_tree,aes(x=V1,y=V2))+geom_point()+labs(x="Number of terminal nodes",
                                                   y="Number of misclassified points")

## Solution to exercise 3

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

## Solution to exercise 4

## To read in external data we use the read.csv() function. 

iris_data<-read.csv("Iris.csv") ## To use this the file MUST BE in saved in the same location as the R script.

iris_data<-read.csv("C://Users/your_file_path/Iris.csv") ## You can also use the whole file path

## Once the data is read in, from here the code is the same as exercise 2.

## Solution to exercise 5

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
