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
    theta0<- matrix(1,nrow=1,ncol=N)
    for (t in 1:T){
      xt<-X[t,]
      Dt <- diag(sqrt(abs(c(theta0))))
      D <- outer(diag(Dt),diag(Dt)) 
      At <- At + tcrossprod(xt,xt)
      InvA <-  chol2inv(chol(diag(a,N) + D * At))
      AAt<- D * InvA 
      theta0<- crossprod(AAt,bt) 
      pred[t] <- crossprod(as.matrix(theta0), xt)
      bt <- bt + (Y[t] * xt)
      theta0 <- crossprod(AAt,bt) 
    }
    res<-postResample(pred = pred, obs = Y)
    stats<- as.matrix(res)
    quant<-quantile(as.matrix(Y)-as.matrix(pred),probs=c(.25,.50,.75))
    
    return(list(predictions=pred,performance=stats,quantiles=quant))
  }
}
