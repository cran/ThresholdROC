varVarEq <-
function(k1,k2){
n1<-length(k1); n2<-length(k2)
est<-2*(varPooled(k1,k2))^2/(n1+n2-1)
return(est)
}
