library(ISLR)
library(class)
library(ggplot2)
library(cluster) 
library(factoextra)
library(dplyr)

attach(iris)

iris[,1:4] <- scale(iris[,1:4])
setosa<- rbind(iris[iris$Species=="setosa",])
versicolor<- rbind(iris[iris$Species=="versicolor",])
virginica<- rbind(iris[iris$Species=="virginica",])


ind <- sample(1:nrow(setosa), nrow(setosa)*0.8)
iris.train<- rbind(setosa[ind,], versicolor[ind,], virginica[ind,])
iris.test<- rbind(setosa[-ind,], versicolor[-ind,], virginica[-ind,])
iris[,1:4] <- scale(iris[,1:4])


error <- c()

for (i in 1:25){
  knn.fit <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k = i)
  error[i] = 1- mean(knn.fit == iris.test$Species)
}

ggplot(data = data.frame(error), aes(x = 1:25, y = error)) +
  geom_line(color = "Blue",size=2)+labs(x="Number of clusters")+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16))

df<-iris[,-5]

silhouette_score <- function(k){
  km <- kmeans(df, centers = k,nstart = 5)
  ss <- silhouette(km$cluster, dist(df)) %>% as.data.frame()
  mean(ss$sil_width)
}

k <- 2:25


avg_sil<-c()

for (i in 2:25){
  avg_sil[i]<-silhouette_score(i)
}

plot(k, type='b', avg_sil[-1], xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)



