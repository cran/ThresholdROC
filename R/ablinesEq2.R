ablinesEq2 <-
function(eq.delta,eq.boot,eq.perc,k1,k2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE),B=500,a=0.05,lty=c(1,1,1),lwd=c(1,1,1),col=c(1,2,3),cex.leg=1,...){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]

if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
}
if(eq.delta==TRUE){ 
ic.eq0<-icDeltaEq2(k1,k2,rho2,costs,a=a)

ic.eq<-c(ic.eq0$LOW,ic.eq0$UP)
abline(v=ic.eq,lty=lty[1],lwd=lwd[1],col=col[1])
legend("topright",paste(c("Delta CI LL:","Delta CI UL:"),round(ic.eq,2)),lty=lty[1],lwd=lwd[1],col=col[1],bty="n",cex=cex.leg)
}
if(eq.boot==TRUE ||  eq.perc==TRUE) {
ic.eq<-icBootEq2(k1,k2,rho2,costs,B=B,a=a)
ic.eq.norm<-c(ic.eq$LOWBOOT,ic.eq$UPBOOT)
ic.eq.perc<-c(ic.eq$LOWPERC,ic.eq$UPPERC)
}
if(eq.boot==TRUE) {
abline(v=ic.eq.norm,lty=lty[2],lwd=lwd[2],col=col[2])
legend("topright",paste(c("Bootstrap SE CI LL:","Bootstrap SE CI UL:"),round(ic.eq.norm,2)),lty=lty[2],lwd=lwd[2],col=col[2],bty="n",inset=c(0,0.1),cex=cex.leg)
}
if(eq.perc==TRUE) {
abline(v=ic.eq.perc,lty=lty[3],lwd=lwd[3],col=col[3])
legend("topright",paste(c("Percentile CI LL:","Percentile CI UL:"),round(ic.eq.perc,2)),lty=lty[3],lwd=lwd[3],col=col[3],bty="n",inset=c(0,0.2),cex=cex.leg) 
}
}
