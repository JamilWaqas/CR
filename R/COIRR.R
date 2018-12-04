
#' COIRR
#'
#' @param This functions takes input data, labels and scalar a>0.
#' @return predictions, RMSE, MAE, and quantiles of COIRR.
#' @export
#' @examples
#' X<-matrix(rexp(200, rate=.1), ncol=20)
#' Y<-rnorm(10)
#' COIRR(X,Y,a=1)


COIRR<-function(X,Y,a){
  if(a<=0){
    print("a must be a positive number")
  }else{
    X<-as.matrix(X)
    Y<-as.matrix(Y)
    T<-nrow(X)
    N<-ncol(X)
    bt<- matrix(0,ncol=1,nrow=N)
    At<- diag(0,N)
    pred<- matrix(0,nrow=T,ncol=1)
    theta0<- rep(1,N)
    theta<- matrix(0,nrow = T,ncol=N)
    for (t in 1:T){
      Dt <- diag(abs(as.numeric(theta0)))
      At <- At + (X[t,] %*% t(X[t,]))
      InvA <-  chol2inv(chol(a*diag(N) + (sqrt(Dt) %*% At %*% sqrt(Dt))))
      AAt<- sqrt(Dt) %*%InvA%*% sqrt(Dt)
      theta0<- AAt%*%bt
      theta[t,] <- t(as.vector(theta0))
      pred[t] <- t(as.matrix(theta0)) %*% X[t,]
      bt <- bt + (Y[t] * X[t,])
      theta0 <- AAt %*% bt
    }
    res<-postResample(pred = pred, obs = Y)
    stats<- as.matrix(res)[c(1,3),]
    quant<-quantile(Y-pred,probs=c(.25,.50,.75))

    return(list(predictions=pred,performance=stats,quantiles=quant))
  }
}
