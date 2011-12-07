VarThr3 <-
function(k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0), 3,3,byrow=TRUE),Thr){
c11<-costs[1,1]; c12<-costs[1,2]; c13<-costs[1,3]; c21<-costs[2,1]; c22<-costs[2,2]; c23<-costs[2,3]; c31<-costs[3,1]; c32<-costs[3,2]; c33<-costs[3,3]; 
 rho1<-rho[1]; rho2<-rho[2]; rho3<-rho[3]
sk11<-VDel3st(k1,k2,k3,rho,costs,Thr)[1]
sk12<-SecondDer3st(k1,k2,k3,rho,costs,Thr)[1]
sk21<-VDel3st(k1,k2,k3,rho,costs,Thr)[2]
sk22<-SecondDer3st(k1,k2,k3,rho,costs,Thr)[2]
  ret1<-sk11/sk12^2
  ret2<-sk21/sk22^2

re<-list(VAR1=ret1,VAR2=ret2)

class(re)<-"VARMAK" # Asignar clase al objeto

return(re)
}
