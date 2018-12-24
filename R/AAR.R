#' AAR
#'
#' @param This functions takes input data, labels and scalar a>0.
#' @return predictions, RMSE, MAE, and quantiles of AAR.
#' @export
#' @examples
#' X<-matrix(rexp(200, rate=.1), ncol=20)
#' Y<-rnorm(10)
#' AAR(X,Y,a=1)

AAR<- function(X,Y,a){
  if(a<=0){
    print("a must be a positive number")
  }else{
    X<- as.matrix(X)
    Y<-as.matrix(Y)
    N<- ncol(X)
    T<- nrow(X)
    bt<-matrix(0,ncol=1,nrow=N)
    At<-diag(1/a, N)
    pred<-matrix(0,ncol=1,nrow=T)
    for(t in 1:T){
      xt<-X[t,]
      pred[t,]<- tcrossprod(crossprod(bt,At),xt) / as.numeric(crossprod(xt,crossprod(At,xt))+1)
      At<- At + tcrossprod(xt,xt)
      At<- At - (tcrossprod(crossprod(At,xt),crossprod(At,xt)) / as.numeric(crossprod(xt,crossprod(At,xt))+1))
      bt<- bt + (Y[t,]*xt)
    }
    res<-postResample(pred = pred, obs = Y)
    stats<- as.matrix(res)
    quant<-quantile(Y-pred,probs=c(.25,.50,.75))
    
    return(list(predictions=pred,performance=stats,quantiles=quant))
  }
}
