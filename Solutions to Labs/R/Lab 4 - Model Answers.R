##############################################################################
####                                                                      ####
####          Week 4  - Deep Learning Exercises Solutions                 ####
####                                                                      ####
##############################################################################

## The required libraries

library(tidyverse)
library(keras3)

## Solution to exercise 1

## Load in the required dataset from the keras package

mnist <- dataset_mnist()
X_train <- mnist$train$x
X_test <- mnist$test$x
y_train <- mnist$train$y
y_test <- mnist$test$y

X_train <- array_reshape(X_train, c(nrow(X_train), 784))
X_train <- X_train / 255
X_test <- array_reshape(X_test, c(nrow(X_test), 784))
X_test <- X_test / 255

y_train <- to_categorical(y_train, num_classes = 10)
y_test <- to_categorical(y_test, num_classes = 10)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 256, activation = "relu",
              input_shape=c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units=128, activation = "relu") %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units = 10, activation = "softmax")

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>%
  fit(X_train, y_train, epochs = 2, batch_size = 64, validation_split = 0.2)

model %>%
  evaluate(X_test, y_test)
