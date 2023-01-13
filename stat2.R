dataset2=matrix(c(
  88,69,70,74,70,86,76,74,
  58,84,68,79,75,83,93,78,
  92,85,69,67,81,79,97,83,
  77,78,84,68,80,69,87,69,
  81,79,88,96,77,83,75,91,
  86,72,89,90,79,73,83,88,
  90,86,82,66,80,75,81,82,
  67,94,75,69,91,85,76,80), nrow=8, ncol = 8, byrow = TRUE)

par(mfrow =c(1,1))
#dx<-density(dataset1,bw=bw.nrd0(dataset1))
dx<-density(dataset2, n = 64,bw = "nrd0")
plot(dx,lwd=3,col="blue",main="Silverman's Rule Of Thumb",font.lab=2)
par(mfrow =c(1,1))
# create sequence of bandwidths, compute and plot CV function
dy<-density(dataset2, n = 64,bw = "ucv")
plot(dy,lwd=3,col="green",main="Least Squares Cross Validation",font.lab=2)
# get the mean value from data given
mean(dataset2,na.rm=TRUE)
m <-mean(dataset2,na.rm=TRUE)
#get the Standard Deviation 
sd(dataset2,na.rm = TRUE)
Sd <-sd(dataset2,na.rm = TRUE)
# Get the standard Distribution 
dnorm(dataset2,m,Sd)
x=dataset2
y<-dnorm(dataset2,m,Sd)

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

