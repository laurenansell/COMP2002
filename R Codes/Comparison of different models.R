library(ISLR2)
library(glmnet)

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

x<-scale(model.matrix(Salary~. -1, data=Hitters)) ## matrix containing the predictors

y<-Hitters$Salary ## Vector containing the response to be predicted


cvfit<-cv.glmnet(x[-testid,],y[-testid],type.measure = "mae")

cpred<-predict(cvfit,x[testid,],s="lambda.min")
mean(abs(y[testid]-cpred))

## Fitting the neural network

