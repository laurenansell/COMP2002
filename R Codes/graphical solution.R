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
  scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  geom_text()+annotate("text",label="5x+2y=30",x=2,y=12)+
  annotate("text",label="5x+7y=35",x=1,y=5)+
  annotate("text",label="2x+5y=20",x=7.5,y=2)


y_ob_1<-(200-40*x)/50
y_ob_2<-(400-40*x)/50
y_ob_3<-(600-40*x)/50

ob1<-as.data.frame(cbind(x,y_ob_1))
ob2<-as.data.frame(cbind(x,y_ob_2))
ob3<-as.data.frame(cbind(x,y_ob_3))


ggplot()+geom_line(data=const1,aes(x=x,y=y1),linewidth=1.5)+
  geom_line(data=const2,aes(x=x,y=y2),col="red",linewidth=1.5)+
  geom_line(data=const3,aes(x=x,y=y3),col="blue",linewidth=1.5)+
  scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  geom_text()+annotate("text",label="5x+2y=30",x=2,y=12)+
  annotate("text",label="5x+7y=35",x=1,y=5)+
  annotate("text",label="2x+5y=20",x=7.5,y=2)+
  geom_line(data=ob1,aes(x=x,y=y_ob_1),linetype=4,linewidth=1.5)+
  geom_line(data=ob2,aes(x=x,y=y_ob_2),linetype=4,linewidth=1.5)+
  geom_line(data=ob3,aes(x=x,y=y_ob_3),linetype=4,linewidth=1.5)+
  geom_text()+annotate("text",label="40x+50y=200",x=1,y=2)+
  annotate("text",label="40x+50y=400",x=1,y=8.5)+
  annotate("text",label="40x+50y=600",x=1.2,y=10)

