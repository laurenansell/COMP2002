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

## First create an empty dataframe with 51 rows nd no columns to store the data

y<-data.frame(matrix(NA,ncol=0,nrow = 51)) 

for (i in 1:10) {
  
  result<-optimise(BitFlipMutation,one_max,greaterThanOrEqual,50,10)
  Fitness<-result$A
  y<-cbind(y,Fitness)
  names(y)[i]<-paste0("run_",i)## Need to make sure we create names for the columns
  
}

y_ave=y %>% mutate(ave_fit=rowMeans(pick(where(is.numeric))))

Iteration<-c(1:nrow(y_ave))

y_ave<-cbind(y_ave,Iteration)

ggplot(y_ave, aes(x=Iteration, y=ave_fit))+geom_line(col="blue",linewidth=2)+
  ylab("Fitness (maximum)")

## Solution to exercise 3

## Create an operator that generates a new random solution:
random<-function(x){
  N<-length(x)
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


Random_result<-data.frame(matrix(NA,ncol=0,nrow = 201)) 

for (i in 1:10) {
  
  result<-optimise(random,one_max,greaterThanOrEqual,200,50)
  Fitness<-result$A
  Random_result<-cbind(Random_result,Fitness)
  names(Random_result)[i]<-paste0("run_",i)## Need to make sure we create names for the columns
  
}

Random_ave=Random_result %>% mutate(ave_fit=rowMeans(pick(where(is.numeric))))

Blockflip_result<-data.frame(matrix(NA,ncol=0,nrow = 201)) 

for (i in 1:10) {
  
  result<-optimise(BlockFlip,one_max,greaterThanOrEqual,200,50)
  Fitness<-result$A
  Blockflip_result<-cbind(Blockflip_result,Fitness)
  names(Blockflip_result)[i]<-paste0("run_",i)## Need to make sure we create names for the columns
  
}

Blockflip_ave=Blockflip_result %>% mutate(ave_fit=rowMeans(pick(where(is.numeric))))

Bitflip_result<-data.frame(matrix(NA,ncol=0,nrow = 201)) 

for (i in 1:10) {
  
  result<-optimise(BitFlipMutation,one_max,greaterThanOrEqual,200,50)
  Fitness<-result$A
  Bitflip_result<-cbind(Bitflip_result,Fitness)
  names(Bitflip_result)[i]<-paste0("run_",i)## Need to make sure we create names for the columns
  
}

Bitflip_ave=Bitflip_result %>% mutate(ave_fit=rowMeans(pick(where(is.numeric))))

Iteration<-c(1:nrow(Bitflip_ave))

mutation_results<-as.data.frame(cbind(Iteration, Bitflip_ave$ave_fit,
                                      Blockflip_ave$ave_fit,Random_ave$ave_fit))
mutation_results<-mutation_results %>% rename("Bitflip"="V2","Blockflip"="V3","Random"="V4")

mutation_results<-mutation_results %>% pivot_longer(!Iteration,names_to = "Mutation",
                                        values_to = "Fitness")

ggplot(mutation_results,aes(x=Iteration,y=Fitness,col=Mutation))+
  geom_line(linewidth=2)+labs(y="Fitness (maximise)",title = "OneMax")

## Solution to exercise 4

leading_ones<-function(x){
  
  count<-0
  
  for (i in 1:length(x)) {
    if (x[i] ==1) count<-count+1 else {break}
    
  }
  
  return(count)
  
}



Random_result<-data.frame(matrix(NA,ncol=0,nrow = 201)) 

for (i in 1:10) {
  
  result<-optimise(random,leading_ones,greaterThanOrEqual,200,50)
  Fitness<-result$A
  Random_result<-cbind(Random_result,Fitness)
  names(Random_result)[i]<-paste0("run_",i)## Need to make sure we create names for the columns
  
}

Random_ave=Random_result %>% mutate(ave_fit=rowMeans(pick(where(is.numeric))))

Blockflip_result<-data.frame(matrix(NA,ncol=0,nrow = 201)) 

for (i in 1:10) {
  
  result<-optimise(BlockFlip,leading_ones,greaterThanOrEqual,200,50)
  Fitness<-result$A
  Blockflip_result<-cbind(Blockflip_result,Fitness)
  names(Blockflip_result)[i]<-paste0("run_",i)## Need to make sure we create names for the columns
  
}

Blockflip_ave=Blockflip_result %>% mutate(ave_fit=rowMeans(pick(where(is.numeric))))

Bitflip_result<-data.frame(matrix(NA,ncol=0,nrow = 201)) 

for (i in 1:10) {
  
  result<-optimise(BitFlipMutation,leading_ones,greaterThanOrEqual,200,50)
  Fitness<-result$A
  Bitflip_result<-cbind(Bitflip_result,Fitness)
  names(Bitflip_result)[i]<-paste0("run_",i)## Need to make sure we create names for the columns
  
}

Bitflip_ave=Bitflip_result %>% mutate(ave_fit=rowMeans(pick(where(is.numeric))))

Iteration<-c(1:nrow(Bitflip_ave))

mutation_results<-as.data.frame(cbind(Iteration, Bitflip_ave$ave_fit,
                                      Blockflip_ave$ave_fit,Random_ave$ave_fit))
mutation_results<-mutation_results %>% rename("Bitflip"="V2","Blockflip"="V3","Random"="V4")

mutation_results<-mutation_results %>% pivot_longer(!Iteration,names_to = "Mutation",
                                                    values_to = "Fitness")

ggplot(mutation_results,aes(x=Iteration,y=Fitness,col=Mutation))+
  geom_line(linewidth=2)+labs(y="Fitness (maximise)",title="Leading Ones")

