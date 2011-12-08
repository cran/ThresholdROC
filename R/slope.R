slope <-
function(rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2,byrow=TRUE)){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
beta<-((1-rho2)/rho2)*((c.f.pos-c.t.neg)/(c.f.neg-c.t.pos))
return(beta)
}
