cost3fun <-
function(x,k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0), 3,3,byrow=TRUE)){
 rho1<-rho[1]; rho2<-rho[2]; rho3<-rho[3]
c11<-costs[1,1]; c12<-costs[1,2]; c13<-costs[1,3]; c21<-costs[2,1]; c22<-costs[2,2]; c23<-costs[2,3]; c31<-costs[3,1]; c32<-costs[3,2]; c33<-costs[3,3]; 
n1<-length(k1); n2<-length(k2); n3<-length(k3)
ret<-(n1+n2+n3)*(rho1*(c11*pnorm(x[1],mean(k1),sd(k1))+c12*(pnorm(x[2],mean(k1),sd(k1))-pnorm(x[1],mean(k1),sd(k1)))+c13*(1-pnorm(x[2],mean(k1),sd(k1))))+rho2*(c21*pnorm(x[1],mean(k2),sd(k2))+c22*(pnorm(x[2],mean(k2),sd(k2))-pnorm(x[1],mean(k2),sd(k2)))+c23*(1-pnorm(x[2],mean(k2),sd(k2))))+rho3*(c31*pnorm(x[1],mean(k3),sd(k3))+c32*(pnorm(x[2],mean(k3),sd(k3))-pnorm(x[1],mean(k3),sd(k3)))+c33*(1-pnorm(x[2],mean(k3),sd(k3)))))
return(ret)
}
