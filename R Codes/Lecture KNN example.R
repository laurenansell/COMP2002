library(ISLR)
library(class)
library(ggplot2)
library(dplyr)

attach(Caravan)


Caravan_scaled <- scale(Caravan[,-86])

ind <- sample(1:nrow(Caravan_scaled), nrow(Caravan_scaled)*0.8)
caravan.train<-Caravan_scaled[ind,]
caravan.train.cl<-Caravan[ind,86]
caravan.test<- Caravan_scaled[-ind,]
caravan.test.cl<-Caravan[-ind,86]

## Fitting the knn

knn_model<-knn(train = caravan.train, 
               test = caravan.train,
               cl=caravan.train.cl,
               k=4)


tab = table(Predicted=knn_model, Actual = caravan.train.cl)
tab

error <- c()

for (i in 1:20){
  knn.fit <- knn(train = caravan.train, test = caravan.test,
                 cl = caravan.train.cl, 
                 k = i)
  error[i] = 1- mean(knn.fit == caravan.test.cl)
}

ggplot(data = data.frame(error), aes(x = 1:20, y = error)) +
  geom_line(color = "Blue",linewidth=2)+labs(x="Number of neighbours")+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))
