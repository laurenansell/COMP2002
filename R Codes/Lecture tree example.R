library(tidyverse)
library(rpart)
library(rpart.plot)

## Load in the data

penguins<-read.csv("./R Codes/penguins.csv")

## Check the structure of the data

str(penguins)

## Three of the variables are characters, we can change these to factors

penguins$species<-as.factor(penguins$species)
penguins$island<-as.factor(penguins$island)
penguins$sex<-as.factor(penguins$sex)

## Create the tree

penguin_tree<-rpart(species~., data = penguins)

## Plot the tree

rpart.plot(penguin_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
