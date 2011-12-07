derVarEq <-
function(k1,k2,rho2,costs){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
est<-log(slope(rho2,costs))/(mean(k2)-mean(k1))
return(est)
}
