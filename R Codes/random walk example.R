RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}
# mu is the drift

P1<-RW(100,10,0,0.0004)
P2<-RW(100,10,0,0.0004)
P3<-RW(100,10,0,0.0004)
P4<-RW(100,10,0,0.0004)
plot(P1, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(9.7,10.3),
     typ='l', col="red")
par(new=T)  #to draw in the same plot
plot(P2, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(9.7,10.3),
     typ='l', col="blue")
par(new=T)  #to draw in the same plot
plot(P3, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(9.7,10.3),
     typ='l', col="darkgreen")
par(new=T)  #to draw in the same plot
plot(P4, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(9.7,10.3),
     typ='l', col="gold")
