library(e1071)

attach(iris)

penguins<-read.csv("./R Codes/penguins.csv")

## remove the NA values

penguins<-na.omit(penguins)

penguins$species<-as.factor(penguins$species)
penguins$island<-as.factor(penguins$island)
penguins$sex<-as.factor(penguins$sex)

svmfit <- svm(species ~ . , data = penguins, kernel ="linear" )

plot(svmfit, data=penguins,
     bill_length_mm~body_mass_g,
     slice = list(flipper_length_mm=25, bill_depth_mm=0.1) 
)


set.seed(2)
def.subset <- sample(333, 260)      

penguins_training<-penguins[def.subset,]

penguins_test<-penguins[-def.subset,]

svmfit_training<-svm(species ~ . , data = penguins_training, kernel ="linear" )

plot(svmfit_training, data=penguins_training,
     bill_length_mm~body_mass_g,
     slice = list(flipper_length_mm=20, bill_depth_mm=0.1) 
)


summary(svmfit_training)

tab_training<-table(Predicted=svmfit_training$fitted,Actual=penguins_training$species)
tab_training

pred<-predict(svmfit_training,penguins_test)

plot(svmfit_training, data=penguins_test,
     bill_length_mm~body_mass_g,
     slice = list(flipper_length_mm=20, bill_depth_mm=0.1) 
)
tab = table(Predicted=pred, Actual = penguins_test$species)
tab
