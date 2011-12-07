icDeltaEq2 <-
function(k1,k2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE),a=0.05){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
cut<-thresEq2(k1,k2,rho2,costs)[[1]]
stdev<-sqrt(varDeltaEq2(k1,k2,rho2,costs))

ic1<-cut+qnorm(a/2)*stdev
ic2<-cut+qnorm(1-a/2)*stdev

beta<-((1-rho2)/rho2)*((c.f.pos-c.t.neg)/(c.f.neg-c.t.pos))

ic<-list(LOW=ic1,UP=ic2,CL=1-a,COSTS=costs,SLOPE=beta,PREV=rho2)

class(ic)<-"DELTAEQ" # Asignar clase al objeto

return(ic)
}
