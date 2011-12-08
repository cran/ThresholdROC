control <-
function(par1.1,par1.2,par2.1,par2.2,rho2,costs){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
ctrl<- (par2.1-par1.1)^2+2*log((par2.2/par1.2)*slope(rho2,costs))*(par2.2^2-par1.2^2)
return(ctrl)
}
