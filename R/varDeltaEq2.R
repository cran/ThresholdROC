varDeltaEq2 <-
function(k1,k2,rho2,costs){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
}
n1<-length(k1); n2<-length(k2)
est<-(derMeanDisEq(k1,k2,rho2,costs))^2*varMeanEq(k1,k2,n2)+(derMeanNDisEq(k1,k2,rho2,costs))^2*varMeanEq(k1,k2,n1)+(derVarEq(k1,k2,rho2,costs))^2*varVarEq(k1,k2)
return(est)
}
