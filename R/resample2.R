resample2 <-
function(k1,k2,B){
n1<-length(k1); n2<-length(k2)
t0<-matrix(sample(k1,n1*B,replace=TRUE),nrow=n1)
t1<-matrix(sample(k2,n2*B,replace=TRUE),nrow=n2)
t<-list(t0,t1)
return(t)
}
