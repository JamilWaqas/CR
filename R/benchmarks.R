#' benchmarks
#'
#' @param This functions takes input data and labels. Use of function requires 'glmnet' and 'caret' package.
#' @return predictions, RMSE, MAD, and quantiles of lm, ee and lasso.
#' @export
#' @examples
#' benchmarks(X,Y)


benchmarks<-function(X,Y){
  lambdas <- 50^seq(3, -2, by = -.1)

  model<-lm(Y~X)
  lm.pred<-as.matrix(predict(model))

  ridge.mod <- glmnet(X, Y, alpha = 0, lambda = 1, thresh = 1e-12)
  cv_fit <- cv.glmnet(X, Y, alpha = 0, lambda = lambdas)
  opt_lambda <- cv_fit$lambda.min
  ridge.pred <- predict(ridge.mod, s=opt_lambda, newx=X)

  lasso.mod <- glmnet(X, Y, alpha = 1, lambda = 1, thresh = 1e-12)
  cv_fit <- cv.glmnet(X, Y, alpha = 1, lambda = lambdas)
  opt_lambda <- cv_fit$lambda.min
  lasso.pred<- predict(lasso.mod, s=opt_lambda, newx=X)

  pred<-as.data.frame(cbind(lm.pred,ridge.pred,lasso.pred))
  colnames(pred)<- c("lm","rr","lasso")

  lm<-postResample(pred = pred$lm, obs = Y)
  rr<-postResample(pred = pred$rr, obs = Y)
  lasso<-postResample(pred = pred$lasso, obs = Y)

  stats<- as.matrix(rbind(lm,rr,lasso))[,c(1,3)]
  rownames(stats)<-c("lm","rr","lasso")

  qlm<-quantile(Y-pred$lm,probs=c(.25,.50,.75))
  qrr<-quantile(Y-pred$rr,probs=c(.25,.50,.75))
  qlasso<-quantile(Y-pred$lasso,probs=c(.25,.50,.75))

  quant<- rbind(qlm,qrr,qlasso)
  rownames(quant)<-c("lm","rr","lasso")

  return(list(predictions=pred,performance=stats,quantiles=quant))
}
