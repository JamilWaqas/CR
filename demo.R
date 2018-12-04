install.packages("devtools")
library(devtools)
install_github("JamilWaqas/CR")
install.packages("caret")
install.packages("glmnet")
install.packages("microbenchmark")
library(CR)


load(file = "data/Temp.rda")
load(file = "data/ISE.rda")
load(file = "data/NO2.rda")
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


benchmarks(Xtemp,Ytemp)
AAR(Xtemp,Ytemp,1)
ORR(Xtemp,Ytemp,1)
OSLOG(Xtemp,Ytemp,1)
COIRR(Xtemp,Ytemp,1)


#NO2 Data
data<- NO2
data<- data[order(data[,8],data[,7]),]
data<- as.matrix(data[,-c(8)])
X<- as.matrix(data[,-c(1)])
X<-data.frame(X)
XNO2<-as.matrix(cbind(rep(1,500),X))
YNO2<- as.matrix(data[,1])

benchmarks(XNO2,YNO2)
AAR(XNO2,YNO2,1)
ORR(XNO2,YNO2,1)
OSLOG(XNO2,YNO2,1)
COIRR(XNO2,YNO2,1)

#ISE
data<- ISE
XISE<- as.matrix(data[,-c(1,3)])
YISE<- as.matrix(data[,3])

benchmarks(XISE,YISE)
AAR(XISE,YISE,0.001953125)
ORR(XISE,YISE,1.525879e-05)
OSLOG(XISE,YISE,0.0001220703)
COIRR(XISE,YISE,0.0009765625)
