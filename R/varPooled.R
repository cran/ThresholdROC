varPooled <-
function(k1,k2){
n1<-length(k1); n2<-length(k2);
pool<-((n1-1)*var(k1)+(n2-1)*var(k2))/(n1+n2-2)
return(pool)
}
