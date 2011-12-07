th2st3 <-
function(k1,rho1,n1,n2,n3,c11,c12,Th){
  par1.1<-mean(k1); par1.2<-sd(k1)
  der<--(n1+n2+n3)/(sqrt(2*pi)*par1.2^2)*rho1*(c11-c12)*exp(-(Th-par1.1)^2/(2*par1.2^2))*(-1+((Th-par1.1)^2/par1.2^2))
return(der)
}
