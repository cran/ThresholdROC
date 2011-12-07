derMeanNDisEq <-
function(k1,k2,rho2,costs){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
est<-1/2-(varPooled(k1,k2)*log(slope(rho2,costs)))/((mean(k2)-mean(k1))^2)
return(est)
}
