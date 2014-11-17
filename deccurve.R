detcurve <- function(x,a,m,sd){
  if(x<=m){
    return <- 1/(1+exp(-a*(x-m+sd)))
  }else{
    return <- 1/(1+exp(a*(x-m-sd)))
  }
}