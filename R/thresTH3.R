thresTH3 <-
function(q1,q21,q23,q3,switch1,switch2,switch3,par1.1,par1.2,par2.1,par2.2,par3.1,par3.2,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0), 3,3,byrow=TRUE)){
p1<-quant(switch1)(q1,par1.1,par1.2)
p21<-quant(switch2)(q21,par2.1,par2.2)
p23<-quant(switch2)(q23,par2.1,par2.2)
p3<-quant(switch3)(q3,par3.1,par3.2)
cut1<-uniroot(Der1,c(p1,p21),tol=0.00000001,switch1,switch2,switch3,par1.1,par1.2,par2.1,par2.2,par3.1,par3.2,rho,costs)$root
cut2<-uniroot(Der2,c(p23,p3),tol=0.00000001,switch1,switch2,switch3,par1.1,par1.2,par2.1,par2.2,par3.1,par3.2,rho,costs)$root

re<-list(T1=cut1,T2=cut2,PREV=rho,COSTS=costs)

class(re)<-"THRESTH3" # Asignar clase al objeto

return(re)
}
