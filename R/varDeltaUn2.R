varDeltaUn2 <-
function(k1,k2,rho2,costs){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
}
n1<-length(k1); n2<-length(k2)
ctrl<-sqroot(k1,k2,rho2,costs) 
if(ctrl<0) est<-NA
else
est<-(derMeanDisUn(k1,k2,rho2,costs))^2*varMeanUn(k2,n2)+(derMeanNDisUn(k1,k2,rho2,costs))^2*varMeanUn(k1,n1)+(derVarDisUn(k1,k2,rho2,costs))^2*varVarUn(k2,n2)+(derVarNDisUn(k1,k2,rho2,costs))^2*varVarUn(k1,n1)
return(est)
}
