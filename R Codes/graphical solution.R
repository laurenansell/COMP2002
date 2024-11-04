library(tidyverse)

x<-c(0:10)
y1<-(30-5*x)/2
y2<-(35-5*x)/7
y3<-(20-2*x)/5

const1<-as.data.frame(cbind(x,y1))
const2<-as.data.frame(cbind(x,y2))
const3<-as.data.frame(cbind(x,y3))

ggplot()+geom_line(data=const1,aes(x=x,y=y1),linewidth=1.5)+
  geom_line(data=const2,aes(x=x,y=y2),col="red",linewidth=1.5)+
  geom_line(data=const3,aes(x=x,y=y3),col="blue",linewidth=1.5)+
  ylim(0,16)+xlim(0,11)
