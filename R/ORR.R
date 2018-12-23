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
    At<-diag(a, N)
    pred<-matrix(0,ncol=1,nrow=T)
    for(t in 1:T){
      xt<-X[t,]
      pred[t,]<- tcrossprod(xt,crossprod(bt,chol2inv(chol((At)))))
      At<- At + tcrossprod(xt,xt)
      bt<- bt + as.numeric(Y[t,]* xt)
    }
    res<-postResample(pred = pred, obs = Y)
    stats<- as.matrix(res)
    quant<-quantile(Y-pred,probs=c(.25,.50,.75))
    
    return(list(predictions=pred,performance=stats,quantiles=quant))
  }
}
