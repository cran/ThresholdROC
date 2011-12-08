thresTH2 <-
function(q1,q2,switch1,switch2,par1.1,par1.2,par2.1,par2.2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE)){

c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(par1.1>par2.1){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-par2.1; par2.1<-par1.1; par1.1<-g
f<-par2.2; par2.2<-par1.2; par1.2<-f
}
p1<-quant(switch1)(q1,par1.1,par1.2)
p2<-quant(switch2)(q2,par2.1,par2.2)
cut.t<-uniroot(DensRatio2,c(p1,p2),tol=0.00000001,switch1,switch2,par1.1,par1.2,par2.1,par2.2,rho2,costs)$root

beta<-((1-rho2)/rho2)*((c.f.pos-c.t.neg)/(c.f.neg-c.t.pos))

re<-list(THRES=cut.t,PREV=rho2,COSTS=costs,SLOPE=beta)

class(re)<-"thresTH2" # Asignar clase al objeto

return(re)
}
