
#' ORR
#'
#' @param This functions takes input data, labels and scalar a>0.
#' @return predictions, RMSE, MAE, and quantiles of ORR.
#' @export
#' @examples
#' X<-matrix(rexp(200, rate=.1), ncol=20)
#' Y<-rnorm(10)
#' ORR(X,Y,a=1)

ORR<- function(X,Y,a){
  if(a<=0){
    print("a must be a positive number")
  }else{
    X<- as.matrix(X)
    Y<-as.matrix(Y)
    N<- ncol(X)
    T<- nrow(X)
    bt<-matrix(0,ncol=1,nrow=N)
    At<-a * diag(1, N)
    pred<-matrix(0,ncol=1,nrow=T)
    for(t in 1:T){
      pred[t,]<-  t(bt)%*%chol2inv(chol((At)))%*%X[t,]
      At<- At + (X[t,] %*% t(X[t,]))
      bt<- bt + as.numeric(Y[t,]*X[t,])
    }
    res<-postResample(pred = pred, obs = Y)
    stats<- as.matrix(res)[c(1,3),]
    quant<-quantile(Y-pred,probs=c(.25,.50,.75))

    return(list(predictions=pred,performance=stats,quantiles=quant))
  }
}

