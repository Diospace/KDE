dataset1=matrix(c(
  2.2, 4.1, 3.5,4.5,3.2, 3.7,3.0, 2.6,
  3.4,1.6, 3.1, 3.3, 3.8, 3.1, 4.7,3.7,
  2.5,4.3, 3.4, 3.6, 2.9, 3.3, 3.9, 3.1,
  3.3, 3.1, 3.7,4.4, 3.2, 4.1,1.9, 3.4,
  4.7,3.8, 3.2, 2.6,3.9,3.0, 4.2, 3.5), nrow=5, ncol = 8, byrow = TRUE)

par(mfrow =c(1,1))
#dx<-density(dataset1,bw=bw.nrd0(dataset1))
dx<-density(dataset1, n = 40,bw = "nrd0")
plot(dx,lwd=3,col="blue",main="Silverman's Rule Of Thumb",font.lab=2)
par(mfrow =c(1,1))
# create sequence of bandwidths, compute and plot CV function
dy<-density(dataset1, n = 40,bw = "ucv")
plot(dy,lwd=3,col="green",main="Least Squares Cross Validation",font.lab=2)
# get the mean value from data given
mean(dataset1,na.rm=TRUE)
m <-mean(dataset1,na.rm=TRUE)
#get the Standard Deviation 
sd(dataset1,na.rm = TRUE)
Sd <-sd(dataset1,na.rm = TRUE)
# Get the standard Distribution 
dnorm(dataset1,m,Sd)
x=dataset1
y<-dnorm(dataset1,m,Sd)

plot(x,y,lwd=3,col="red",main="Standard Distribution ",font.lab=2)


bias1<-(mean(dx$y)-y[1])^2
bias1
# bias h1 of density using "Least Squares Cross Validation" bandwidth selection
bias2=(mean(dy$y)-y[1])^2
bias2

# variance bias h1 of density using "Silverman's Rule Of Thumb" bandwidth selection
var1=mean((dx$y- mean(dx$y))^2) #Create )
var1
# variance bias h1 of density using "Least Squares Cross Validation" bandwidth selection
var2=mean((dx$y -mean(dx$y))^2)
var2
#mean square error for dx
MSE1=bias1+var1
MSE1

splxy1 = splinefun(1, (MSE1))
#means integral Square Error for bandwidth "Silverman's Rule 
integrate(splxy1, lower = min(x),upper = max(x))
#mean sauare error
MSE2=bias2 + var2
MSE2
splxy2= splinefun(1, (MSE2))
#means integral Square Error for bandwidth "Least Squares Cross Validation"
integrate(splxy2, lower = min(x),upper = max(x))


