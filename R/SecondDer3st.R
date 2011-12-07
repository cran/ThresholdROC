SecondDer3st <-
function(k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0), 3,3,byrow=TRUE),Thr){
par1.1<-mean(k1); par2.1<-mean(k2); par3.1<-mean(k3); par1.2<-sd(k1); par2.2<-sd(k2); par3.2<-sd(k3)
n1<-length(k1); n2<-length(k2); n3<-length(k3)
rho1<-rho[1]; rho2<-rho[2]; rho3<-rho[3]
c11<-costs[1,1]; c12<-costs[1,2]; c13<-costs[1,3]; c21<-costs[2,1]; c22<-costs[2,2]; c23<-costs[2,3]; c31<-costs[3,1]; c32<-costs[3,2]; c33<-costs[3,3]; 

  der1<-(n1+n2+n3)/sqrt(2*pi)*( rho1*(c12-c11)*((Thr[1]-par1.1)/par1.2^3)*exp(-(Thr[1]-par1.1)^2/(2*par1.2^2))+
                       rho2*(c22-c21)*((Thr[1]-par2.1)/par2.2^3)*exp(-(Thr[1]-par2.1)^2/(2*par2.2^2))+
                       rho3*(c32-c31)*((Thr[1]-par3.1)/par3.2^3)*exp(-(Thr[1]-par3.1)^2/(2*par3.2^2)) )
  der2<-(n1+n2+n3)/sqrt(2*pi)*( rho1*(c13-c12)*((Thr[2]-par1.1)/par1.2^3)*exp(-(Thr[2]-par1.1)^2/(2*par1.2^2))+
                       rho2*(c23-c22)*((Thr[2]-par2.1)/par2.2^3)*exp(-(Thr[2]-par2.1)^2/(2*par2.2^2))+
                       rho3*(c33-c32)*((Thr[2]-par3.1)/par3.2^3)*exp(-(Thr[2]-par3.1)^2/(2*par3.2^2)) )
return(c(der1,der2))
}
