ablines3 <-
function(param,paramval,boot,bootval,perc,percval,k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0),3,3,byrow=TRUE),start,B=500,a=0.05,lty=c(1,1,1),lwd=c(1,1,1),col=c(1,2,3),cex.leg=1,...){

if(param==TRUE) {
ic.param<-icParam3(k1,k2,k3,rho,costs,a=0.05,start=start)

T1<-c(ic.param$LOW1,ic.param$UP1)
T2<-c(ic.param$LOW2,ic.param$UP2)
abline(v=T1,lty=lty[1],lwd=lwd[1],col=col[1])
abline(v=T2,lty=lty[1],lwd=lwd[1],col=col[1])
}
if(paramval==TRUE) {
#legend("topleft",paste(c("Parametric CI LL:","Parametric CI UL:"), c(round(T1,2),round(T2,2))),lty=lty[1],lwd=lwd[1],col=col[1],bty="n",inset=c(0,0.1),cex=cex.leg)
legend("topleft",paste ("Parametric CI T1:","(",round(T1[1],2),",",round(T1[2],2),")"),lty=lty[1],lwd=lwd[1],col=col[1],bty="n",inset=c(0,0.1),cex=cex.leg)
legend("topleft",paste ("Parametric CI T2:","(",round(T2[1],2),",",round(T2[2],2),")"),lty=lty[1],lwd=lwd[1],col=col[1],bty="n",inset=c(0,0.15),cex=cex.leg)
}
if(boot==TRUE || perc==TRUE) {
ic.boot<-icBoot3(k1,k2,k3,rho,costs,B=500,a=0.05,start=start)
}
if(boot==TRUE) {
T3<-c(ic.boot$LOWNORM1,ic.boot$UPNORM1)
T4<-c(ic.boot$LOWNORM2,ic.boot$UPNORM2)
abline(v=T3,lty=lty[2],lwd=lwd[2],col=col[2])
abline(v=T4,lty=lty[2],lwd=lwd[2],col=col[2])
}
if(bootval==TRUE) {
legend("topleft",paste ("Bootstrap CI T1:","(",round(T3[1],2),",",round(T3[2],2),")"),lty=lty[2],lwd=lwd[2],col=col[2],bty="n",inset=c(0,0.2),cex=cex.leg)
legend("topleft",paste ("Bootstrap CI T2:","(",round(T4[1],2),",",round(T4[2],2),")"),lty=lty[2],lwd=lwd[2],col=col[2],bty="n",inset=c(0,0.25),cex=cex.leg)
}
if(perc==TRUE) {
T5<-c(ic.boot$LOWPERC1,ic.boot$UPPERC1)
T6<-c(ic.boot$LOWPERC2,ic.boot$UPPERC2)
abline(v=T5,lty=lty[3],lwd=lwd[3],col=col[3])
abline(v=T6,lty=lty[3],lwd=lwd[3],col=col[3])
}
if(percval==TRUE) {
legend("topleft",paste ("Bootstrap CI T1:","(",round(T5[1],2),",",round(T5[2],2),")"),lty=lty[3],lwd=lwd[3],col=col[3],bty="n",inset=c(0,0.3),cex=cex.leg)
legend("topleft",paste ("Bootstrap CI T2:","(",round(T6[1],2),",",round(T6[2],2),")"),lty=lty[3],lwd=lwd[3],col=col[3],bty="n",inset=c(0,0.35),cex=cex.leg)
}
}
