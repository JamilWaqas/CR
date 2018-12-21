#' time
#'
#' @param This functions takes input data, labels and scalar a>0. Use of function require 'microbenchmark'
#' @return time statistics taken for AAR, ORR, OSLOG and COIRR.
#' @export
#' @examples
#' X<-matrix(rexp(200, rate=.1), ncol=20)
#' Y<-rnorm(10)
#' time(X,Y,a=1)

time<- function(X,Y,a){
mbm<-microbenchmark(AAR = CR::AAR(X,Y,a),ORR=CR::ORR(X,Y,a),OSLOG=CR::OSLOG(X,Y,a),COIRR=CR::COIRR(X,Y,a))
return(mbm)
}
