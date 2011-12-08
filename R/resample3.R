resample3 <-
function(k1,k2,k3,B=500){
n1<-length(k1); n2<-length(k2); n3<-length(k3)
t1<-matrix(sample(k1,n1*B,replace=TRUE),nrow=n1)
t2<-matrix(sample(k2,n2*B,replace=TRUE),nrow=n2)
t3<-matrix(sample(k3,n3*B,replace=TRUE),nrow=n3)
t<-list(t1,t2,t3)
return(t)
}
