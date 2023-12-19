library(e1071)

attach(iris)

iris$Species<-as.factor(iris$Species)

svmfit <- svm(Species ~ . , data = iris, kernel ="linear" )

plot(svm_model, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)


set.seed(2)
def.subset <- sample(150, 120)      

iris_training<-iris[def.subset,]

iris_test<-iris[-def.subset,]

svmfit_training<-svm(Species ~ . , data = iris_training, kernel ="linear" )

plot(svmfit_training, data=iris_training,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)

summary(svmfit_training)

tab_training<-table(Predicted=svmfit_training$fitted,Actual=iris_training$Species)
tab_training

pred<-predict(svmfit_training,iris_test)

plot(svmfit_training, data=iris_test,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)
tab = table(Predicted=pred, Actual = iris_test$Species)
tab
