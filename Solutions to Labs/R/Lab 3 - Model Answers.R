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

## Solution to Exercise 1

## First read in the data

digits_data<-read_csv("../R Code/mnist_train.csv")## You will need to change this file path

digits_data$label<-as.factor(digits_data$label)

names(digits_data)<-make.names(names(digits_data))

model = neuralnet(
  label~.,
  data=digits_data,
  hidden=c(4,2),
  linear.output = FALSE
)

pred <- predict(model, digits_data)

## Confusion matrix
labels <- c("0", "1", "2","3","4","5","6","7","8","9")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  dplyr:: select(2) %>%
  unlist()

table(digits_data$label, prediction_label)

## Solution to Exercise 2

## First read in the dataset

housing_data<-read.csv("./R Codes/housing.csv")

## The dataset fetched by Python has 8 numeric attributes and the target, so we will 
## remove the ocean_proximity variable. This dataset also has slightly different data,
## it contains total rooms not the average and the total number of households per block 
## not the average occupancy.

housing_data<-dplyr::select(housing_data,-ocean_proximity)

## Plot the median income against the house age (these are the first two inputs in the 
## Python dataset)

ggplot(housing_data,aes(x=median_income,y=housing_median_age))+
  geom_point(aes(col=median_house_value))

## Solution to Exercise 3

## In R, we use the scale function to scale values.

housing_data_scaled<-as.data.frame(scale(housing_data))

## Solution to Exercise 4

## Remove any missing values

housing_data_scaled<-na.omit(housing_data_scaled)

## Train the model

inputs<-housing_data_scaled[,-9]

targets<-housing_data_scaled$median_house_value

MLP_model<-mlp(x=inputs,y=targets)

MLP_predictions<-predict(MLP_model,inputs)

## Calculate the Mean Absolute Error

mae(targets,predicted = MLP_predictions)

