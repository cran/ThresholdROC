ablinesEmp2 <-
function(emp.boot,emp.perc,k1,k2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE),B=500,a=0.05,lty=c(1,1),lwd=c(1,1),col=c(2,3),cex.leg=1,...){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
}
ic.lim<-icEmp2(k1,k2,rho2,costs,B=B,a=a)
if(emp.boot==TRUE) {
ic.emp.norm<-c(ic.lim$LOWBOOT,ic.lim$UPBOOT)
abline(v=ic.emp.norm, lty=lty[1], lwd=lwd[1], col=col[1])
legend("topright",paste(c("Bootstrap SE CI LL:","Bootstrap SE CI UL:"),round(ic.emp.norm,2)),lty=lty[1],lwd=lwd[1],col=col[1],bty="n",inset=c(0,0.1),cex=cex.leg)
}
if(emp.perc==TRUE) {
ic.emp.perc<-c(ic.lim$LOWPERC,ic.lim$UPPERC)
abline(v=ic.emp.perc,lty=lty[2], lwd=lwd[2], col=col[2])
legend("topright",paste(c("Percentile CI LL:","Percentile CI UL:"),round(ic.emp.perc,2)),lty=lty[2],lwd=lwd[2],col=col[2],bty="n",inset=c(0,0.2),cex=cex.leg) 
}
}
