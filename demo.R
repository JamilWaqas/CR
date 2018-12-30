fried<-Fried
sample <- 1:floor(.25*nrow(fried))
train <- as.matrix(fried[sample,])
test  <- as.matrix(fried[-sample,])
Xtrain<- as.matrix(train[,1:10])
Ytrain<-as.matrix(train[,11])
Xtest<- as.matrix(test[,1:10])
Ytest<- as.matrix(test[,11])

print(AAR(Xtrain,Ytrain,0.1)$Weights)
print(ORR(Xtrain,Ytrain,0.5)$Weights)
print(OSLOG(Xtrain,Ytrain,0.5)$Weights)
print(COIRR(Xtrain,Ytrain,0.5)$Weights)

print(AAR(Xtest,Ytest,0.1)$quantiles)
print(ORR(Xtest,Ytest,0.5)$quantiles)
print(OSLOG(Xtrain,Ytrain,0.5)$quantiles)
print(COIRR(Xtest,Ytest,0.5)$quantiles)

print(AAR(Xtest,Ytest,0.1)$performance)
print(ORR(Xtest,Ytest,0.5)$performance)
print(OSLOG(Xtest,Ytest,0.5)$performance)
print(COIRR(Xtest,Ytest,0.5)$performance)

print(OSLOG(Xtest[,-c(3,6,7,8,9,10)],Ytest,0.5)$performance)
print(COIRR(Xtest[,-c(3,6,7,8,9,10)],Ytest,0.5)$performance)

print(benchmarks(Xtest,Ytest)$performance)
print(benchmarks(Xtest,Ytest)$quantiles)

Xtest[1000,3]<- 500
Xtest[1000,1]<- 500

print(OSLOG(Xtest,Ytest,0.5)$performance)
print(COIRR(Xtest,Ytest,0.5)$performance)


data<- ISE
sample <- 1:floor(.25*nrow(data))
train <- data[sample, ]
test  <- data[-sample, ]
Xtrain <- as.matrix(train[,-c(1,3)])
Ytrain <-as.matrix(train[,3])
Xtest <- as.matrix(test[,-c(1,3)])
Ytest <- as.matrix(test[,3])

#tuningCOIRR<-tuneCOIRR(0.00000001,0.00001,0.000000001,Xtrain,Ytrain)
#tuningORR<-tuneORR(0.00000001,0.00001,0.000000001,Xtrain,Ytrain)
#tuningOSLOG<-tuneOSLOG(0.00000001,0.00001,0.000000001,Xtrain,Ytrain)
#tuningAAR<-tuneAAR(0.00000001,0.00001,0.000000001,Xtrain,Ytrain)

#print(AAR(Xtrain,Ytrain,tuningAAR$optimum)$Weights)
#print(ORR(Xtrain,Ytrain,tuningORR$optimum)$Weights)
#print(OSLOG(Xtrain,Ytrain,tuningOSLOG$optimum)$Weights)
#print(COIRR(Xtrain,Ytrain,tuningORR$optimum)$Weights)


perfISE<-rbind(t(CR::ORR(Xtest,Ytest,1e-05)$performance),t(CR::AAR(Xtest,Ytest,1e-05)$performance),t(CR::OSLOG(Xtest[,-c(5)],Ytest,1e-05)$performance),t(CR::COIRR(Xtest[,-c(5)],Ytest,1e-05)$performance))
colnames(perfISE)<-c("RMSE","R2","MAE")
rownames(perfISE)<-c("orr","aar","oslog","coirr")
print(perfISE)
print(benchmarks(Xtest,Ytest)$performance)


quant<-rbind(t(CR::ORR(Xtest,Ytest,1e-05)$quantiles),t(CR::AAR(Xtest,Ytest,1e-05)$quantiles),t(CR::OSLOG(Xtest[,-c(5)],Ytest,1e-05)$quantiles),t(CR::COIRR(Xtest[,-c(5)],Ytest,1e-05)$quantiles))
colnames(quant)<-c("25%","50%","75%")
rownames(quant)<-c("orr","aar","oslog","coirr")
print(quant)

print(benchmarks(Xtest,Ytest)$quantiles)

F16<- F16
devtools::use_data(F16, F16)
sample <- 1:floor(.25*nrow(F16))
train <- as.matrix(F16[sample,])
test  <- as.matrix(F16[-sample,])
Xtrain<- as.matrix(train[,1:40])
Ytrain<-as.matrix(train[,41])
Xtest<- as.matrix(test[,1:40])
Ytest<- as.matrix(test[,41])


perf<-rbind(t(CR::ORR(Xtest[,-c(26,30,32,34)],as.matrix(Ytest),1e-05)$performance),t(CR::AAR(Xtest[,-c(26,30,32,34)],as.matrix(Ytest),1e-05)$performance),t(CR::OSLOG(Xtest[,c(1:10,39)],as.matrix(Ytest),1e-05)$performance),t(CR::COIRR(Xtest[,c(1:10,39)],as.matrix(Ytest),1e-05)$performance))
colnames(perf)<-c("RMSE","R2","MAE")
rownames(perf)<-c("orr","aar","oslog","coirr")
print(perf)
print(benchmarks(Xtest,Ytest)$performance)


quant<-rbind(t(CR::ORR(Xtest[,-c(26,30,32,34)],as.matrix(Ytest),1e-05)$quantiles),t(CR::AAR(Xtest[,-c(26,30,32,34)],as.matrix(Ytest),1e-05)$quantiles),t(CR::OSLOG(Xtest[,c(1:10,39)],as.matrix(Ytest),1e-05)$quantiles),t(CR::COIRR(Xtest[,c(1:10,39)],as.matrix(Ytest),1e-05)$quantiles))
colnames(quant)<-c("25%","50%","75%")
rownames(quant)<-c("orr","aar","oslog","coirr")
print(quant)

print(benchmarks(Xtest,Ytest)$quantiles)
