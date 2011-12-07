SSRatio <-
function(par1.1,par1.2,par2.1,par2.2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE)){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(par1.2==par2.2){
eps<-((par2.1-par1.1)^2-2*par1.2^2*log(slope(rho2,costs)))/((par2.1-par1.1)^2+2*par1.2^2*log(slope(rho2,costs)))
}
else{
eps<-sqrt(((parDerMeanDisUn(par1.1,par1.2,par2.1,par2.2,rho2,costs)^2*par2.2^2+2*parDerVarDisUn(par1.1,par1.2,par2.1,par2.2,rho2,costs)^2*par2.2^4))/( (parDerMeanNonDisUn(par1.1,par1.2,par2.1,par2.2,rho2,costs)^2*par1.2^2+2*parDerVarNonDisUn(par1.1,par1.2,par2.1,par2.2,rho2,costs)^2*par1.2^4)))
}
return(eps)
}
