Der2 <-
function(p,switch1,switch2,switch3,par1.1,par1.2,par2.1,par2.2,par3.1,par3.2,rho,costs) {
rho1<-rho[1]; rho2<-rho[2]; rho3<-rho[3]
c11<-costs[1,1]; c12<-costs[1,2]; c13<-costs[1,3]; c21<-costs[2,1]; c22<-costs[2,2]; c23<-costs[2,3]; c31<-costs[3,1]; c32<-costs[3,2]; c33<-costs[3,3]; 
eq<-(rho1*(c12-c13)*dens(switch1)(p,par1.1,par1.2)+rho2*(c22-c23)*dens(switch2)(p,par2.1,par2.2)+rho3*(c32-c33)*dens(switch3)(p,par3.1,par3.2))
return(eq)
}
