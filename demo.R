install.packages("devtools")
library(devtools)
install_github("JamilWaqas/CR")
install.packages("caret")
install.packages("glmnet")
install.packages("microbenchmark")
library(CR)
library(MASS)
library(glmnet)
library(caret)
library(microbenchmark)
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

print(AAR(Xtemp,Ytemp,1)$performance)
print(ORR(Xtemp,Ytemp,1)$quantiles)

print(OSLOG(Xtemp,Ytemp,1)$performance)
print(OSLOG(Xtemp,Ytemp,1)$quantiles)

print(COIRR(Xtemp,Ytemp,1)$performance)
print(COIRR(Xtemp,Ytemp,1)$quantiles)

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

print(AAR(XNO2,YNO2,1)$performance)
print(AAR(XNO2,YNO2,1)$quantiles)

print(ORR(XNO2,YNO2,1)$performance)
print(ORR(XNO2,YNO2,1)$quantiles)

print(OSLOG(XNO2,YNO2,1)$performance)
print(OSLOG(XNO2,YNO2,1)$quantiles)

print(COIRR(XNO2,YNO2,1)$performance)
print(COIRR(XNO2,YNO2,1)$quantiles)
#ISE
data<- ISE
XISE<- as.matrix(data[,-c(1,3)])
YISE<- as.matrix(data[,3])

print(benchmarks(XISE,YISE)$performance)
print(benchmarks(XISE,YISE)$quantiles)

print(AAR(XISE,YISE,0.001953125)$performance)
print(AAR(XISE,YISE,0.001953125)$quantiles)

print(ORR(XISE,YISE,0.0001220703)$performance)
print(ORR(XISE,YISE,0.0001220703)$quantiles)

print(COIRR(XISE,YISE,0.0009765625)$performance)
print(COIRR(XISE,YISE,0.0009765625)$quantiles)

print(OSLOG(XISE,YISE,1.525879e-05)$performance)
print(OSLOG(XISE,YISE,1.525879e-05)$quantiles)

