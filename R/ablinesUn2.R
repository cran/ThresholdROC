ablinesUn2 <-
function(un.delta,un.boot,un.perc,k1,k2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE),B=500,a=0.05,lty=c(1,1,1),lwd=c(1,1,1),col=c(1,2,3),cex.leg=1,...){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
}
if(un.delta==TRUE){ 
ic.un0<-icDeltaUn2(k1,k2,rho2,costs,a=a)
ic.un<-c(ic.un0$LOW,ic.un0$UP)
abline(v=ic.un,lty=lty[1],lwd=lwd[1],col=col[1])
legend("topright",paste(c("Delta CI LL:","Delta CI UL:"),round(ic.un,2)),lty=lty[1],lwd=lwd[1],col=col[1], bty="n",cex=cex.leg)
}
if(un.boot==TRUE ||  un.perc==TRUE) {
ic.un<-icBootUn2(k1,k2,rho2,costs,B=B,a=a)
ic.un.norm<-c(ic.un$LOWBOOT,ic.un$UPBOOT)
ic.un.perc<-c(ic.un$LOWPERC,ic.un$UPPERC)
}
if(un.boot==TRUE) {
abline(v=ic.un.norm,lty=lty[2],lwd=lwd[2],col=col[2])
legend("topright",paste(c("Bootstrap SE CI LL:","Bootstrap SE CI UL:"),round(ic.un.norm,2)),lty=lty[2],lwd=lwd[2],col=col[2],bty="n",inset=c(0,0.1),cex=cex.leg)
}
if(un.perc==TRUE) {
abline(v=ic.un.perc,lty=lty[3],lwd=lwd[3],col=col[3])
legend("topright",paste(c("Percentile CI LL:","Percentile CI UL:"),round(ic.un.perc,2)),lty=lty[3],lwd=lwd[3],col=col[3],bty="n",inset=c(0,0.2),cex=cex.leg) 
}
}
