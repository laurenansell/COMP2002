##############################################################################
####                                                                      ####
####                 Week 1  - R Exercises Solutions                      ####
####                                                                      ####
##############################################################################



## Solution to exercise 1

x<-c(3,6,2,9,12,8,22) # Creates an object called x containing these values. 
## We can replace the <- with = 
length(x)

x[4] ## Selects just the fourth element

x<-c(x,45) ## Adds a new element to the end of x
length(x)

x_remove<-x[-2] ## Removes the second element from x

x_select<-x[c(1,3:8)] ## Keeps all but the second element of x. 
## The notation a:b means keep all the integer values from a to b (inclusive)

## Solution to exercise 2

## Load in the required libraries

library(tidyverse)
library(pracma)

x<-linspace(-pi,pi,100)  # Generate the 100 points
y<-sin(x)

df<-as.data.frame(cbind(x,y)) # Combine the vectors into a dataframe for plotting

ggplot(df, aes(x=x,y=y))+geom_line(linewidth=2,col="blue")+
  labs(title = "y=sin(x)")



## Solution to exercise 3

uniform<-runif(100) # Generate 100 uniform random points.
normal<-rnorm(100)  # Generate 100 normal random points.

par(mfcol=c(1,2)) # creates one row with 2 columns

boxplot(uniform,xlab="Uniform")
boxplot(normal,xlab="Normal")

## Solution to extension tasks

par(mfcol=c(2,1)) # creates two rows with one column

hist(uniform)
hist(normal)

## Solution to exercise 4

fibonacci=function(n){
  fibseq<-c(0,1)
  
  for (i in 3:n){
    fibseq_new<-fibseq[i-1]+fibseq[i-2]
    fibseq[i]<-fibseq_new
  }
  return(fibseq)
}

fibonacci(10)


## Solution to exercise 5

# Input: numeric vector of two or more elements
bubble_sort <- function(x) {
  swap_performed <- TRUE
  # Repeat the algorithm until no more swaps are performed
  while (swap_performed) {
    swap_performed <- FALSE
    # Check if any swaps are necessary
    for (i in 1:(length(x) - 1)) {
      if (x[i] > x[i + 1]) {
        # Swap elements that are not in increasing order
        tmp <- x[i]
        x[i] <- x[i + 1]
        x[i + 1] <- tmp
        # Now record that a swap was performed
        # This requests another pass through the while loop
        swap_performed <- TRUE
      }
    }
  }
  # Output: the vector sorted in increasing order
  return(x)
}

x<-c(1,5,3,2,4,6)

bubble_sort(x)

