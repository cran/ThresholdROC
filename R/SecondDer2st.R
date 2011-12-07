SecondDer2st <-
function(k1,k2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE),Thr){
par1.1<-mean(k1); par2.1<-mean(k2); par1.2<-sd(k1); par2.2<-sd(k2);
n1<-length(k1); n2<-length(k2)
beta<-slope(rho2,costs)
de<-(n1+n2)*rho2*(costs[1,1]-costs[2,2])
  der<-de/sqrt(2*pi)* (((Thr-par2.1)/par2.2^3)*exp(-(Thr-par2.1)^2/(2*par2.2^2))-beta*((Thr-par1.1)/par1.2^3)*exp(-(Thr-par1.1)^2/(2*par1.2^2)))
return(der)
}
