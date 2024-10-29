##############################################################################
####                                                                      ####
####   Week 6  - Multi-objective Optimisation Exercises Solutions         ####
####                                                                      ####
##############################################################################


## Load the required libraries

library(tidyverse)
library(smoof)
library(mco)

## Solution to exercise 1


problem<-makeZDT1Function(10)

y<-data.frame()

N<-10000

for (i in 1:N) {
  
  x<-runif(10)
  result<-problem(x)
  y<-rbind(y,result)

}


names(y)[1]<-"X1"
names(y)[2]<-"X2"

visualizeParetoOptimalFront(makeZDT1Function(10))+
  geom_point(data=y, aes(x=X1,y=X2),col="red")


## Solution to exercise 2

dominates<-function(u,v){
  
  isTRUE (all(u<=v) & any(u<v))
  
}


sol1<-c(1,2)
sol2<-c(4,2)
sol3<-c(1,3)
sol4<-c(3,4)

dominates(sol1,sol2)
dominates(sol2,sol1)
dominates(sol3,sol4)
dominates(sol4,sol3)
dominates(sol3,sol2)
dominates(sol2,sol3)


updateArchive<-function(A,y){
  
  idx<-c()
  
  Dominate<-c()
  
  for (i in 1:nrow(A)) {
    if (dominates(y,A[i,])==TRUE) idx<-rbind(i)
  }
  
  A<- if (isTRUE(length(idx)==0)) {A} else {A[-idx,]}
  
  for (i in 1:nrow(A)) {
    result<-dominates(A[i,],y)
    Dominate<-c(Dominate,result)}
  
  if(any(Dominate==TRUE)){A}else{A<-rbind(A,y)} 
  
  return(A)
  
}

A<-data.frame()
A<-rbind(A,y[1,])

for (i in 2:N) {
  A_new<-updateArchive(A,y[i,])
  A<-A_new
}

names(A)[1]<-"X1"
names(A)[2]<-"X2"

ggplot()+geom_point(data=y,aes(x=X1,y=X2))+
  geom_point(data=A,aes(x=X1,y=X2),col="red")


## Solution to exercise 3

evolve<-function(mutation, func, compare, x, y, A){
  xp<-mutation(x)
  yp<-func(xp)
  y<-rbind(y,yp)
  x<- xp
  A<-updateArchive(A,tail(y,n=1))
  return(list(x=x, y=y, A=A))
}

optimise<-function(mutation, func ,compare, ngens){
  x<-runif(10)
  y<-data.frame()
  y<-rbind(y,func(x))
  A<-data.frame()
  A<-rbind(A,y[1,])
  result<-evolve(mutation, func, compare, x, y, A)
  for (i in 1:ngens) {
    result_new<-evolve(mutation, func, compare, result$x, result$y, result$A)
    result<-result_new
  }
  return(result)
}

random<-function(x){
  N<-length(x)
  x<-runif(N)
  return(x)
}

test_run<-optimise(random,problem,dominates,1000)

solution<-test_run$y
Archive<-test_run$A

ggplot()+geom_point(data=solution,aes(x=solution[,1],y=solution[,2]))+
  geom_point(data=Archive,aes(x=Archive[,1],y=Archive[,2]),col="red")


## Solution to exercise 4

## swap mutation

swap_mutation<-name <- function(x) {
  
  idx1<-sample(1:length(x),1)
  idx2<-sample(1:length(x),1)
  x<-replace(x, c(idx1, idx2), x[c(idx2, idx1)])
  
  return(x)
  
}


swap_run<-optimise(swap_mutation,problem,dominates,1000) 

swap_solution<-swap_run$y
swap_Archive<-swap_run$A

ggplot()+geom_point(data=solution,aes(x=solution[,1],y=solution[,2]))+
  geom_point(data=Archive,aes(x=Archive[,1],y=Archive[,2]),col="red")+
  geom_point(data = swap_Archive,aes(x=swap_Archive[,1],y=swap_Archive[,2]),
             col="blue")


scramble_mutation<-function(x){
  
  new_idxs<-sample(1:length(x),length(x))
  
  x_new<-x[new_idxs]
  
}

scramble_run<-optimise(scramble_mutation,problem,dominates,1000)

scramble_solution<-scramble_run$y
scramble_Archive<-scramble_run$A

visualizeParetoOptimalFront(makeZDT1Function(10))+
  geom_point(data=solution,aes(x=solution[,1],y=solution[,2]),pch=4)+
  geom_point(data=Archive,aes(x=Archive[,1],y=Archive[,2]),col="red")+
  geom_point(data = swap_Archive,aes(x=swap_Archive[,1],y=swap_Archive[,2]),
             col="blue")+
  geom_point(data = scramble_Archive,aes(x=scramble_Archive[,1],
                                         y=scramble_Archive[,2]),
             col="gold")


