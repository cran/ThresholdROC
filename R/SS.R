SS <-
function(par1.1,par1.2,par2.1,par2.2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE),width,epsilon,a=0.05){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(par1.1>par2.1){
rho2<-1-rho2
epsilon<-1/epsilon
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-par1.1; par1.1<-par2.1; par2.1<-g
f<-par1.2; par1.2<-par2.2; par2.2<-f
}
if(par1.2==par2.2){
est.non.dis<-qnorm(a/2)^2/((width/2)^2*((2*(log(slope(rho2,costs))/(par2.1-par1.1))^2*par1.2^4)/(epsilon+1)+(1/2-(par1.2^2*log(slope(rho2,costs))/(par2.1-par1.1)^2))^2*par1.2^2/epsilon+(1/2+(par1.2^2*log(slope(rho2,costs))/(par2.1-par1.1)^2))^2*par2.1^2))
}
else{
est.non.dis<-(parDerMeanDisUn(par1.1,par1.2,par2.1,par2.2,rho2,costs)^2*par2.2^2/epsilon+parDerMeanNonDisUn(par1.1,par1.2,par2.1,par2.2,rho2,costs)^2*par1.2^2+2*parDerVarDisUn(par1.1,par1.2,par2.1,par2.2,rho2,costs)^2*par2.2^4/epsilon+2*parDerVarNonDisUn(par1.1,par1.2,par2.1,par2.2,rho2,costs)^2*par1.2^4)*qnorm(a/2)^2/((width/2)^2)
}

est.dis<-epsilon*est.non.dis

beta<-((1-rho2)/rho2)*((c.f.pos-c.t.neg)/(c.f.neg-c.t.pos))

re<-list(SS2=est.dis, SS1=est.non.dis, EPS=epsilon, WID=width, CL=1-a, COSTS=costs,SLOPE=beta,PREV=rho2)
class(re)<-"SS" # Asignar clase al objeto
return(re)
}
