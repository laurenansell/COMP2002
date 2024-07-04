## First load in the required library - remember you may need to install first

ibrary(ISLR2) ## data is stored in this library
library(glmnet) ## this package is required to fit the lasso model
library(keras) ## this package is required to fit the neural network

Hitters<-na.omit(Hitters)

n<-nrow(Hitters)

set.seed(2)

ntest<-trunc(n/3)

testid<-sample(1:n,ntest)

## Fitting a linear model to the data

lfit<-lm(Salary~., data = Hitters[-testid,])

lpred<-predict(lfit,Hitters[testid,])
with(Hitters[testid,], mean(abs(lpred-Salary)))

## How long did this take?

time_for_lm <- system.time(lm(Salary~., data = Hitters[-testid,]))
time_for_lm

## Fitting the lasso model

x<-scale(model.matrix(Salary~. -1, data=Hitters)) ## matrix containing the predictors

y<-Hitters$Salary ## Vector containing the response to be predicted


cvfit<-cv.glmnet(x[-testid,],y[-testid],type.measure = "mae")

cpred<-predict(cvfit,x[testid,],s="lambda.min")
mean(abs(y[testid]-cpred))

## How long did this take?

time_for_cv <- system.time(cv.glmnet(x[-testid,],y[-testid],type.measure = "mae"))
time_for_cv


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


## How long did this take?

time_for_nn <- system.time(modnn %>% fit(x[-testid,],y[-testid],epochs = 1500,batch_size = 32,
                                         validation_data = list(x[testid,],y[testid])))
time_for_nn