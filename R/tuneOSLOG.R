#' tuneOSLOG
#'
#' @param This functions takes scalers; start value, end value, increment and input/output of the training set.
#' @return vector of tuning parameter values, and optimum value.
#' @export
#' @examples
#' X<-matrix(rexp(200, rate=.1), ncol=20)
#' Y<-rnorm(10)
#' data<- cbind(Y,X)
#' sample <- 1:floor(.25*nrow(data))
#' train <- data[sample, ]
#' test  <- data[-sample, ]
#' Xtrain<- as.matrix(train[,-1])
#' Ytrain<-scale(as.matrix(train[,1]))
#' Xtest<- as.matrix(test[,-c1])
#' Ytest<- as.matrix(test[,1])
#' tuneOSLOG(0.1,0.2,0.01,Xtrain,Ytrain)

tuneOSLOG<-function(from,to,by,Xtrain,Ytrain,b){
  a <- seq(from,to,by)
  res<-matrix(0,ncol=1,nrow=length(a))
  pb <- txtProgressBar(min = 0, max = length(a), style = 3)
  for(j in 1:length(a)) {
    setTxtProgressBar(pb, j)
    pre<- as.matrix(CR::OSLOG(Xtrain,Ytrain,a[j],b)$predictions)
    res[j] <- sum((pre-Ytrain)^2)
  }
  close(pb)
  parm<-which.min(res)
  opt<-a[parm]
  return(list(result=res,optimum = opt))
}
