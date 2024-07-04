library(ISLR2)

Hitters<-na.omit(Hitters)

n<-nrow(Hitters)

set.seed(2)

ntest<-trunc(n/3)

testid<-sample(1:n,ntest)

## Fitting a linear model to the data

lfit<-lm(Salary~., data = Hitters[-testid,])

lpred<-predict(lfit,Hitters[testid,])
with(Hitters[testid,], mean(abs(lpred-Salary)))


## Fitting the lasso model

