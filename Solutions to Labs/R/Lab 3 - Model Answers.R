## Load in the libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(neuralnet) 
library(MASS)
library(RSNNS)
library(Metrics)
library(nnet)
library(readr)
library(lehuynh)

## Solution to Exercise 1

## First read in the data

iris_data<-read.csv("./Solutions to Labs/R/Iris.csv") ## You will need to change the file path

iris_data$Species<-as.factor(iris_data$Species)

names(iris_data)<-make.names(names(iris_data))

model<-neuralnet(
  Species~.,
  data=iris_data,
  hidden=c(4,2),
  linear.output = FALSE
)

pred <- predict(model, iris_data)

## Confusion matrix
labels <- c( "setosa", "versicolor","virginica")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  dplyr:: select(2) %>%
  unlist()

table(iris_data$Species, prediction_label)

## Using ggplot to create a confusion matrix.

predicted_class<-prediction_label
actual_class<-iris_data$Species

confusion_matrix <- as.data.frame(table(predicted_class, actual_class))

ggplot(data = confusion_matrix,
       mapping = aes(x = actual_class,
                     y = predicted_class)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red")

## Solution to Exercise 2

## First read in the dataset

housing_data<-read.csv("./R Codes/housing.csv")

## The dataset has 8 numeric attributes and the target and one non-numeric, so we will 
## remove the ocean_proximity variable. This dataset also has slightly different data,
## it contains total rooms not the average and the total number of households per block 
## not the average occupancy.

housing_data<-dplyr::select(housing_data,-ocean_proximity)

## Plot the median income against the house age

ggplot(housing_data,aes(x=median_income,y=housing_median_age))+
  geom_point(aes(col=median_house_value))

## Solution to Exercise 3

## In R, we use the minmax() function to scale values.

housing_data_scaled<-as.data.frame(MinMaxScaling(housing_data))

sapply(housing_data, function(housing_data)
  max(housing_data, na.rm=TRUE) - min(housing_data, na.rm=TRUE))
sapply(housing_data_scaled, function(housing_data_scaled)
  max(housing_data_scaled, na.rm=TRUE) - min(housing_data_scaled, na.rm=TRUE))

## Solution to Exercise 4

## Remove any missing values

housing_data_scaled<-na.omit(housing_data_scaled)

## Train the model

## We can fit the model in the same way as we did for the first exercise, 
## however this time we change the argument for linear.output to TRUE.

nn_model<-neuralnet(median_house_value~.,
                    data = housing_data_scaled,
                    hidden = 1,
                    linear.output = TRUE)

nn_predictions<-predict(nn_model, housing_data_scaled)

mae(housing_data_scaled$median_house_value,predicted = nn_predictions)

## There is also the function mlp() which fits a multilayer perceptron
## This is also quicker than the above

inputs<-housing_data_scaled[,-9]

targets<-housing_data_scaled$median_house_value

MLP_model<-mlp(x=inputs,y=targets)

MLP_predictions<-predict(MLP_model,inputs)

## Calculate the Mean Absolute Error

mae(targets,predicted = MLP_predictions)

