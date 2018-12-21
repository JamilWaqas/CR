{r install-more-packages-1, eval = F}
install.packages("devtools", repos = 'https://cran.us.r-project.org')
library(devtools)
install_github("JamilWaqas/CR")
{r install-more-packages-2, eval = F}
install.packages(c("caret", "glmnet", "microbenchmark","MASS"))

library(CR)
library(MASS)
library(glmnet)
library(caret)
library(microbenchmark)


#Simulation
set.seed(123)
mu <- runif(100,0,1)
Sigma <- matrix(.7, nrow=100, ncol=100) + diag(100)*.3
x <- mvrnorm(n=10000, mu=mu, Sigma=Sigma)
w<-c(runif(99,0,3),2.5)
sigma <- 0.4
eps <- rnorm(10000,0,1)
for(i in 2:10000) eps[i] <- 0.15 * eps[i-1] + sqrt(1-0.15^2)*eps[i]
eps <- sigma * eps
y <-  x %*% w  + eps
Y<- y
X<- x


print(benchmarks(X,Y)$performance)
print(AAR(X,Y,1)$performance)
print(ORR(X,Y,1)$performance)
print(COIRR(X,Y,1)$performance)
print(OSLOG(X,Y,1)$performance)


#Effect of range of Y

mu <- runif(100,0,1)
Sigma <- matrix(.7, nrow=100, ncol=100) + diag(100)*.3
x <- mvrnorm(n=10000, mu=mu, Sigma=Sigma)
w<-c(runif(99,0,3),2.5)
sigma <- 0.4
eps <- rnorm(10000,0,1)
for(i in 2:10000) eps[i] <- 0.15 * eps[i-1] + sqrt(1-0.15^2)*eps[i]
eps <- sigma * eps
y <- scale(x %*% w)  + eps
Y<- y
X<-x

print(benchmarks(X,Y)$performance)
print(AAR(X,Y,1)$performance)
print(ORR(X,Y,1)$performance)
print(COIRR(X,Y,1)$performance)
print(OSLOG(X,Y,1)$performance)


#Effect of outlier

X[5000,100]<-737377

print(benchmarks(X,Y)$performance)
print(AAR(X,Y,1)$performance)
print(ORR(X,Y,1)$performance)
print(COIRR(X,Y,1)$performance)
print(OSLOG(X,Y,1)$performance)


#Effect of shrinkage

set.seed(123)
mu <- runif(100,0,1)
Sigma <- matrix(.7, nrow=100, ncol=100) + diag(100)*.3
x <- mvrnorm(n=10000, mu=mu, Sigma=Sigma)
w<-c(runif(98,0,0.0),3.5,2.5)
sigma <- 0.4
eps <- rnorm(10000,0,1)
for(i in 2:10000) eps[i] <- 0.15 * eps[i-1] + sqrt(1-0.15^2)*eps[i]
eps <- sigma * eps
y <-  x %*% w  + eps
Y<- y
X<-x

print(benchmarks(X,Y)$performance)
print(AAR(X,Y,1)$performance)
print(ORR(X,Y,1)$performance)
print(COIRR(X,Y,1)$performance)
print(OSLOG(X,Y,1)$performance)

data("Temp", package="CR")
data("NO2", package="CR")
data("ISE", package="CR")

data1<-Temp
data<- as.matrix(data1[,-c(1,4)])
X<- as.matrix(data[,-c(3)])
x1<-as.numeric(as.factor(X[,1]))
x2<-as.numeric(as.factor(X[,2]))
x3<-as.numeric(as.factor(X[,9]))
X1<-cbind(X[,3:8],x1,x2,x3)
X <- mapply(X1, FUN=as.numeric)
X<- matrix(data=X, nrow(X1),ncol(X1))
X<-X[,-c(5)]
pw<-X[,1]*X[,6]
sp<-X[,1]^2
hum<-exp(X[,1])
hum2<-sin(X[,1])
Xtemp<-cbind(X,pw,sp,hum,hum2)
Ytemp<- as.matrix(as.numeric(data1[,4]))
print(benchmarks(Xtemp,Ytemp)$performance)
print(benchmarks(Xtemp,Ytemp)$quantiles)
aar<-AAR(Xtemp,Ytemp,1)
orr<-ORR(Xtemp,Ytemp,1)
oslog<-OSLOG(Xtemp,Ytemp,1)
coirr<-COIRR(Xtemp,Ytemp,1)

perfTemp<-as.matrix(rbind(aar$performance,orr$performance,oslog$performance,coirr$performance))
perfTemp<-cbind(perfTemp[,1],(perfTemp[,2]))
colnames(perfTemp)<-c("RMSE","MAE")
rownames(perfTemp)<-c("aar","orr","oslog","coirr")
print(perfTemp)

quantTemp<-as.matrix(rbind(aar$quantiles,orr$quantiles,oslog$quantiles,coirr$quantiles))
colnames(quantTemp)<-c("25%","50%","75%")
rownames(quantTemp)<-c("aar","orr","oslog","coirr")
print(quantTemp)
#NO2 Data
data<- NO2
data<- data[order(data[,8],data[,7]),]
data<- as.matrix(data[,-c(8)])
X<- as.matrix(data[,-c(1)])
X<-data.frame(X)
XNO2<-as.matrix(cbind(rep(1,500),X))
YNO2<- as.matrix(data[,1])
print(benchmarks(XNO2,YNO2)$performance)
print(benchmarks(XNO2,YNO2)$quantiles)

aarNO2<-AAR(XNO2,YNO2,1)
orrNO2<-ORR(XNO2,YNO2,1)
oslogNO2<-OSLOG(XNO2,YNO2,1)
coirrNO2<-COIRR(XNO2,YNO2,1)

perfNO2<-as.matrix(rbind(aarNO2$performance,orrNO2$performance,oslogNO2$performance,coirrNO2$performance))
perfNO2<-cbind(perfNO2[,1],(perfNO2[,2]))
colnames(perfNO2)<-c("RMSE","MAE")
rownames(perfNO2)<-c("aar","orr","oslog","coirr")
print(perfNO2)

quantNO2<-as.matrix(rbind(aarNO2$quantiles,orrNO2$quantiles,oslogNO2$quantiles,coirrNO2$quantiles))
colnames(quantNO2)<-c("25%","50%","75%")
rownames(quantNO2)<-c("aar","orr","oslog","coirr")
print(quantNO2)
#ISE
data<- ISE
XISE<- as.matrix(data[,-c(1,3)])
YISE<- as.matrix(data[,3])

print(benchmarks(XISE,YISE)$performance)
print(benchmarks(XISE,YISE)$quantiles)
aarISE<-AAR(XISE,YISE,0.001953125)
orrISE<-ORR(XISE,YISE,0.0001220703)
oslogISE<-OSLOG(XISE,YISE,0.0009765625)
coirrISE<-COIRR(XISE,YISE,1.525879e-05)

perfISE<-as.matrix(rbind(aarISE$performance,orrISE$performance,oslogISE$performance,coirrISE$performance))
perfISE<-cbind(perfISE[,1],(perfISE[,2]))
colnames(perfISE)<-c("RMSE","MAE")
rownames(perfISE)<-c("aar","orr","oslog","coirr")
print(perfISE)

quantISE<-as.matrix(rbind(aarISE$quantiles,orrISE$quantiles,oslogISE$quantiles,coirrISE$quantiles))
colnames(quantISE)<-c("25%","50%","75%")
rownames(quantISE)<-c("aar","orr","oslog","coirr")
print(quantISE)

print(CR::time(Xtemp,Ytemp,1))
print(CR::time(XNO2,YNO2,1))

