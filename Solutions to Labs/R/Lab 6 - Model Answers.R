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


visualizeParetoOptimalFront(makeZDT1Function(10))+
  geom_point(data=y, aes(x=y[,1],y=y[,2]),col="red")


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

  for (i in 1:nrow(A)) {
    if (dominates(y,A[i,])==TRUE) idx<-rbind(i)
  }
  
  A<-A[-idx,]
  
  Dominate<-c()
  
  for (i in 1:nrow(A)) {
    result<-dominates(A[i,],y)
    Dominate<-c(Dominate,result)}
  
  if(any(Dominate==TRUE)){A}else{A<-rbind(A,y)} 
  
  return(A)
  
}



## Solution to exercise 3



## Solution to exercise 4
