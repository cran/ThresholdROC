varMeanEq <-
function(k1,k2,t){
n1<-length(k1); n2<-length(k2)
est<-varPooled(k1,k2)/t
return(est)
}
