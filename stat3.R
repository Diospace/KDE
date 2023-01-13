dataset3=matrix(c(
  1.2,1.35,2.0,1.7,1.6,2.0,2.4,1.8,2.0,2.0,
  1.4,1.35,2.4,1.9,2.4,2.3,2.0,1.4,2.4,1.6,
  2.6,1.2,1.8,1.6,2.0,1.4,2.6,1.9,1.9,1.7,
  2.0,1.6,1.6,2.0,1.4,1.8,1.3,2.0,2.0,1.9,
  1.4,1.2,1.64,2.4,1.6,1.64,1.7,1.3,2.4,2.2,
  1.7,1.6,1.3,1.8,1.8,2.0,1.6,1.9,2.0,1.86,
  1.6,1.2,2.0,1.6,1.2,2.3,1.5,1.42,1.98,1.4,
  1.5,2.0,1.9,1.64,2.0,1.2,1.9,1.47,2.2,1.9,
  1.48,1.4,1.4,1.3,2.2,1.3,2.4,1.4,1.6,1.7,
  1.6,1.7,2.0,1.4,1.8,1.9,2.1,1.9,2.4,1.6,
  2.2,1.6,1.4,2.0,1.9,2.0,2.3,2.0,2.6,2.3), nrow=11, ncol = 10, byrow = TRUE)

par(mfrow =c(1,1))
#dx<-density(dataset1,bw=bw.nrd0(dataset1))

dx<-density(dataset3, n = 110,bw = "nrd0")


plot(dx,lwd=3,col="blue",main="Silverman's Rule Of Thumb",font.lab=2)

par(mfrow =c(1,1))
# create sequence of bandwidths, compute and plot CV function
dy<-density(dataset3, n = 110,bw = "ucv")
#' the Mean integral square Error
splxy = splinefun(dy$x, (dy$y - dnorm(dx$x))^2)
integrate(splxy, lower = min(dy$x), upper = max(dy$x))
plot(dy,lwd=3,col="green",main="Least Squares Cross Validation",font.lab=2)

# get the mean value from data given
mean(dataset3,na.rm=TRUE)
m <-mean(dataset3,na.rm=TRUE)
#get the Standard Deviation 
sd(dataset3,na.rm = TRUE)
Sd <-sd(dataset3,na.rm = TRUE)
# Get the standard Distribution 
dnorm(dataset3,m,Sd)
x=dataset3
y<-dnorm(dataset3,m,Sd)
plot(x,y,lwd=3,col="red",main="Standard Distribution ",font.lab=2)
# bias h1 of density using "Silverman's Rule Of Thumb" bandwidth selection
bias1=dx$y-y
bias1
# bias h1 of density using "Least Squares Cross Validation" bandwidth selection
bias2=dy$y-y
bias2
# variance  bias h1 of density using "Silverman's Rule Of Thumb" bandwidth selection
var1=var(bias1)
var1
# variance  bias h1 of density using "Least Squares Cross Validation" bandwidth selection
var2=var(bias2)
var2
#MISE of bias h1 of density using "Silverman's Rule Of Thumb" bandwidth selection





