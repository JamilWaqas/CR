#' time
#'
#' @param This functions takes input data, labels and scalar a>0. Use of function require 'microbenchmark'
#' @return time statistics taken for AAR, ORR, OSLOG and COIRR.
#' @export
#' @examples
#' time(X,Y,a)

time<- function(X,Y,a){
mbm<-microbenchmark(AAR = AAR(X,Y,a),ORR=ORR(X,Y,a),OSLOG=SLOG(X,Y,a),COIRR=COIRR(X,Y,a))
return(mbm)
}
