## First load in the required library - remember you may need to install first

ibrary(ISLR2) ## data is stored in this library
library(glmnet) ## this package is required to fit the lasso model
library(keras) ## this package is required to fit the neural network
library(ggplot2) ## This package is to plot the results

Hitters<-na.omit(Hitters)

n<-nrow(Hitters)

set.seed(2)

ntest<-trunc(n/3)

testid<-sample(1:n,ntest)

## Fitting a linear model to the data

lfit<-lm(Salary~., data = Hitters[-testid,])

lpred<-predict(lfit,Hitters[testid,])
with(Hitters[testid,], mean(abs(lpred-Salary)))

## Calculate the R squared value

avr_Salary_actual <- mean(Hitters$Salary[testid])

ss_total_lm <- sum((Hitters$Salary[testid] - avr_Salary_actual)^2)

ss_lm <- sum((lpred - Hitters$Salary[testid])^2)

ss_residuals_lm <- sum((Hitters$Salary[testid] - lpred)^2)

r2_lm <- 1 - ss_residuals_lm / ss_total_lm

r2_lm

## How long did this take?

time_for_lm <- system.time(lm(Salary~., data = Hitters[-testid,]))
time_for_lm

## Plot the results

pred<-lpred
actual<-Hitters$Salary[testid]

results_lm<-as.data.frame(cbind(pred,actual))

ggplot(results_lm,aes(x=pred,y=actual))+geom_point(size=2)+
  theme(axis.title = element_text(size=16))

## Fitting the lasso model

x<-scale(model.matrix(Salary~. -1, data=Hitters)) ## matrix containing the predictors

y<-Hitters$Salary ## Vector containing the response to be predicted


cvfit<-cv.glmnet(x[-testid,],y[-testid],type.measure = "mae")

cpred<-predict(cvfit,x[testid,],s="lambda.min")
mean(abs(y[testid]-cpred))

## Calculate the R squared value

ss_cv <- sum((cpred - Hitters$Salary[testid])^2)

ss_residuals_cv <- sum((Hitters$Salary[testid] - cpred)^2)

r2_cv <- 1 - ss_residuals_cv / ss_total_lm

r2_cv

## How long did this take?

time_for_cv <- system.time(cv.glmnet(x[-testid,],y[-testid],type.measure = "mae"))
time_for_cv

## Plot the results

pred<-cpred

results_cv<-as.data.frame(cbind(pred,actual))

ggplot(results_cv,aes(x=pred,y=actual))+geom_point(size=2)+
  theme(axis.title = element_text(size=16))

## Fitting the neural network

modnn<-keras_model_sequential() %>% 
  layer_dense(units=50,activation = "relu",input_shape = ncol(x)) %>% 
  layer_dropout(rate=0.4) %>% 
  layer_dense(units = 1)

modnn %>% compile(loss="mse", optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error"))

history<-modnn %>% fit(x[-testid,],y[-testid],epochs = 1500,batch_size = 32,
                       validation_data = list(x[testid,],y[testid]))

plot(history)

npred<-predict(modnn,x[testid,])
mean(abs(y[testid]-npred))

## Calculate the R squared value

ss_nn <- sum((npred - Hitters$Salary[testid])^2)

ss_residuals_nn <- sum((Hitters$Salary[testid] - npred)^2)

r2_nn <- 1 - ss_residuals_nn / ss_total_lm

r2_nn


## How long did this take?

time_for_nn <- system.time(modnn %>% fit(x[-testid,],y[-testid],epochs = 1500,batch_size = 32,
                                         validation_data = list(x[testid,],y[testid])))
time_for_nn

## Plot the results

pred<-npred

results_nn<-as.data.frame(cbind(pred,actual))

ggplot(results_nn,aes(x=pred,y=actual))+geom_point(size=2)+
  theme(axis.title = element_text(size=16))
