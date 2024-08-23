
library(tidyverse)
library(cluster) 
library(factoextra)
library(mdsr)  
library(scales)

cities <- world_cities[, c('latitude', 'longitude')]


set.seed(2)

km.cities.K6 <- kmeans(cities, centers = 6)

# The cluster assignments of the observations are contained in km.cities$cluster

head(km.cities.K6$cluster)

## To run the kmeans() function in R with multiple initial cluster assignments, we use the 
## nstart argument. If the value of nstart is greater than one, then K-means clustering
## will be performed using multiple random assignments in Step 1 of the algorithm,
## and the kmeans() function will report only the best results. 

set.seed(2)

km.cities.K6 <- kmeans(cities, centers = 6, nstart = 1)  # same as before
km.cities.K6$tot.withinss     
## km.cities.K6$tot.withinss is the total within-cluster sum of squares (sum of squared 
## distances), which we seek to minimize by performing K-means clustering.

## Looking at variability

v <- rep(0, 20)

for(K in 1:20){
  km.cities <- kmeans(cities, centers = K, nstart = 20)
  v[K] <- km.cities$tot.withinss
}

k<-c(1:20)

var_df<-as.data.frame(cbind(k,v))

ggplot(var_df,aes(x=k,y=v))+geom_point(size=3,col="green")+
  labs(x="Number of clusters, K",y="Within-cluster variability")+ 
  scale_y_continuous(labels = label_comma())+
  theme(axis.title = element_text(size=16))


silhouette_score <- function(k){
  km <- kmeans(cities, centers = k,nstart = 1)
  ss <- silhouette(km$cluster, dist(cities,method="euclidean")) %>% as.data.frame()
  mean(ss$sil_width)
}

k <- 2:25


avg_sil<-c()

for (i in 2:25){
  avg_sil[i]<-silhouette_score(i)
}

plot(k, type='b', avg_sil[-1], xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


