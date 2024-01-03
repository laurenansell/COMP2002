library(ggplot2)


## Identity
x<-c(-3:3)
y<-c(-3:3)

df<-as.data.frame(cbind(x,y))

ggplot(df,aes(x=x,y=y))+geom_line(linewidth=2,col="blue")+
  labs(x="a",y="Identity")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size=14))

## sigmoid
x<-seq(from=-3,to=3,by=0.25)
y<-1/(1+exp(-x))

df<-as.data.frame(cbind(x,y))

ggplot(df,aes(x=x,y=y))+geom_line(linewidth=2,col="blue")+
  labs(x="a",y="Sigmoid")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size=14))

## ReLU

x<-c(-3:3)
y<-c(0,0,0,0,1,2,3)

df<-as.data.frame(cbind(x,y))

ggplot(df,aes(x=x,y=y))+geom_line(linewidth=2,col="blue")+
  labs(x="a",y="ReLU")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size=14))
