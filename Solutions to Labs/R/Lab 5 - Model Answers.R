##############################################################################
####                                                                      ####
####            Week 5  - Otimisation Exercises Solutions                 ####
####                                                                      ####
##############################################################################

## Load the required libraries

library(tidyverse)

## Solution to exercise 1

## First define the one max function, this is the sum of all the elements
one_max <- function(x) {sum(x)}

## The bit flip mutation:
BitFlipMutation<-function(x){
  idx<-sample(length(x),1) ## select a random element of the vector
  xp<-x 
  xp[idx]<-abs(1-xp[idx]) ## retrieve the element of the vector and flip the value
  return(xp)
}

## Create the evolve function:
evolve<-function(mutation, func, compare, x, y, A){
  xp<-mutation(x)
  yp<-func(xp)
  y<-if(compare(yp,y)==TRUE) yp else y
  x<- xp
  A<-c(A,y)
  return(list(x=x, y=y, A=A))
}

## Create the greater than or equal to function:
greaterThanOrEqual<-function(u, v){u>=v}

## Now create the optimising function:
optimise<-function(mutation, func ,compare, ngens, D){
  x<-sample(0:1,size=D,replace=TRUE)
  y<-func(x)
  A<-c()
  result<-evolve(mutation, func, compare, x, y, A)
  for (i in 1:ngens) {
    result_new<-evolve(mutation, func, compare, result$x, result$y, result$A)
    result<-result_new
  }
  return(result_new)
}

result_10<-optimise(BitFlipMutation, one_max, greaterThanOrEqual, 50, 10)

## Plotting the results:
Iteration<-c(1:length(result_10$A))
Fitness<-result_10$A
df<-as.data.frame(cbind(Iteration,Fitness))

ggplot(df, aes(x=Iteration, y=Fitness))+geom_line(col="blue",linewidth=2)+
  ylab("Fitness (maximum)")


## Solution to exercise 2


## Solution to exercise 3

## Create an operator that generates a new random solution:
random<-function(N){
  x<-sample(0:1,size=N,replace=TRUE)
  return(x)
  }

## Create an operator that flips all values between two indices
BlockFlip<-function(x){
  idx_1<-sample(length(x),1) ## select the first random element of the vector
  idx_2<-sample(length(x),1) ## select the second random element of the vector
  xp<-x 
  xp[idx_1:idx_2]<-abs(1-xp[idx_1:idx_2]) 
  return(xp)
  
}


