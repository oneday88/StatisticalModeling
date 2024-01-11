###The computation of pi
library(ggplot2)
nSims       <- 10000
xs          <- runif(nSims, min=0, max=1)
ys          <- runif(nSims, min=0, max=1)
inCircle    <- (xs^2+ys^2)<1
Am          <- sum(inCircle)
An  <- n
pi  <- 4*Am/An

dt  <- data.frame(xs,ys,as.factor(inCircle))
ggplot(dt, aes(xs,ys,col=inCircle)) + geom_point()+ggtitle("MC Approximation of Pi =3.14")

###The integral 
nSims       <- 10000
sims        <- rnorm(nSims,mean=3,sd=6)
mc.integral <- sum(sims >= 0 & sims <= 10)/nSims
print(mc.integral)

library(data.table)
library(ggplot2)
set.seed(123)  # for reproducibility
data <- data.table(x = rnorm(100000, mean = 3, sd = 6))
ggplot(data, aes(x = x)) +
    geom_histogram(binwidth=0.2, fill="cornsilk", colour="blue", size=.2)+
     ggtitle("Normal(3,6)")+theme(plot.title = element_text(hjust = 0.5))
ggsave("normal.png", width = 18, height = 10, units = "cm")


### Rejection sampling
# Basic rejection sampling
targFunc    <- function(x) {dbeta(x, 4,8)}

plot(c(0,1), c(0,6),type="n",xlab="x",ylab="")

xv      <- seq(0.001,0.999,0.01); 
fxv     <- dbeta(xv, 4,8);
dt      <- data.frame(xv,fxv)
ggplot(dt, aes(xv,fxv)) + geom_point()+ggtitle("Beta(4,8)")+theme(plot.title = element_text(hjust = 0.5))

nSims       <- 100000
xs          <- runif(nSims, min=0, max=1)
ys          <- runif(nSims, min=0, max=3)
fxs         <- dbeta(xs, 4, 8)

accepted    <- xs[fxs>ys]
dt2          <- data.frame(accepted)
ggplot(dt2, aes(x = accepted)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white")+geom_density(lwd = 1, colour = 4,fill = 4, alpha = 0.25)+ggtitle("rejection sampling result")+theme(plot.title = element_text(hjust = 0.5))

# improved rejection sampling
xv      <- seq(0.001,0.999,0.001);
fxv     <- dbeta(xv, 4,8);
gxv     <- dbeta(xv,2,2);
subdt1  <- data.frame(xv,fxv, 'f(x)')
subdt2  <- data.frame(xv, gxv, 'g(x)')
subdt3  <- data.frame(xv, 2.45*gxv, 'Mg(x)')
setnames(subdt1, c("xv", "yv", 'Func'))
setnames(subdt2, c("xv", "yv", 'Func'))
setnames(subdt3, c("xv", "yv", 'Func'))

ggplot(rbind(subdt1, subdt2,subdt3), aes(xv,yv, col=Func)) + geom_point()+ggtitle("Beta(4,8) vs Beta(2,2)")+theme(plot.title = element_text(hjust = 0.5))

nSims       <- 100000
xs          <- runif(nSims, min=0, max=1)
us          <- runif(nSims, min=0, max=1)

fxs         <- dbeta(xs, 4, 8)
gxs         <- dbeta(xs, 2,2)
M           <- 2.45
accepted2    <- xs[us<fxs/(M*gxs)]
dt4          <- data.frame(accepted2)
ggplot(dt4, aes(x = accepted2)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white")+geom_density(lwd = 1, colour = 4,fill = 4, alpha = 0.25)+ggtitle("improved sampling result")+theme(plot.title = element_text(hjust = 0.5))



kfun    <- function(x){ dbeta(x,4,8) }
# We could specify any positive function here (*)
k0=integrate(f=kfun,lower=0,upper=1)$value
# This calculates the normalising constant
fxv=kfun(xv)/k0; # This ensures f(x) as defined at (*) is a proper density
lines(xv,fxv,lty=1,lwd=3)
c=max(fxv/hxv); c # 2.4472
lines(xv,c*hxv,lty=3,lwd=3)
legend(0,6,c("f(x)","h(x)","c*h(x)"),lty=c(1,2,3),lwd=c(3,3,3))
text(0.07,3,"c = 2.45")
xval=0.4; lines(c(xval,xval),c(0, c*dbeta(xval,2,2)),lty=1,lwd=1)
points(rep(xval,3), c(0,kfun(xval)/k0 ,c*dbeta(xval,2,2)) ,
pch=rep(16,3), cex=rep(1.2,3))
text(0.43,0.05,"P"); text(0.43,2.5,"Q"); text(0.43,3.3,"R");
c(0,kfun(xval)/k0 ,c*dbeta(xval,2,2))
# 0.0000 2.3649 3.5239 2.3649/3.5239 # 0.6711
text(0.6,5.2,"Probability of accepting 0.4 is p(0.4) = f(0.4)/{c*h(0.4)} \n
= {distance P to Q} divided by {distance P to R}\n= 2.365/3.524 = 0.671")
c(0,kfun(xval)/k0 ,c*dbeta(xval,2,2)) # 0.0000 2.3649 3.5239
