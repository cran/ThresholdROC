VDel3st <-
function(k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0), 3,3,byrow=TRUE),Thr){
 n1<-length(k1); n2<-length(k2); n3<-length(k3)
  c11<-costs[1,1]; c12<-costs[1,2]; c13<-costs[1,3]; c21<-costs[2,1]; c22<-costs[2,2]; c23<-costs[2,3]; c31<-costs[3,1]; c32<-costs[3,2]; c33<-costs[3,3]; 
   rho1<-rho[1]; rho2<-rho[2]; rho3<-rho[3]
 ### derivative means T1
d111<-th1st3(k1,rho1,n1,n2,n3,c11,c12,Thr[1]); d121<-th1st3(k2,rho2,n1,n2,n3,c21,c22,Thr[1]); d131<-th1st3(k3,rho3,n1,n2,n3,c31,c32,Thr[1])
   ### derivative standard deviations T1
  d112<-th2st3(k1,rho1,n1,n2,n3,c11,c12,Thr[1]); d122<-th2st3(k2,rho2,n1,n2,n3,c21,c22,Thr[1]); d132<-th2st3(k3,rho3,n1,n2,n3,c31,c32,Thr[1])
   ### derivative means T2
d211<-th1st3(k1,rho1,n1,n2,n3,c12,c13,Thr[2]); d221<-th1st3(k2,rho2,n1,n2,n3,c22,c23,Thr[2]); d231<-th1st3(k3,rho3,n1,n2,n3,c32,c33,Thr[2])
   ### derivative standard deviations T2
  d212<-th2st3(k1,rho1,n1,n2,n3,c12,c13,Thr[2]); d222<-th2st3(k2,rho2,n1,n2,n3,c22,c23,Thr[2]); d232<-th2st3(k3,rho3,n1,n2,n3,c32,c33,Thr[2])
   par1.1<-mean(k1); par2.1<-mean(k2); par3.1<-mean(k3)
     par1.2<-sd(k1); par2.2<-sd(k2); par3.2<-sd(k3)
  V1<-d111^2*(par1.2^2/n1)+d121^2*(par2.2^2/n2)+d131^2*(par3.2^2/n3)+d112^2*(par1.2^2/(2*n1))+d122^2*(par2.2^2/(2*n2))+d132^2*(par3.2^2/(2*n3))
  V2<-d211^2*(par1.2^2/n1)+d221^2*(par2.2^2/n2)+d231^2*(par3.2^2/n3)+d212^2*(par1.2^2/(2*n1))+d222^2*(par2.2^2/(2*n2))+d232^2*(par3.2^2/(2*n3))
return(c(V1,V2))
}
