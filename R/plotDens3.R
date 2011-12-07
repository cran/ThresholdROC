plotDens3 <-
function(thr=TRUE,thrval=TRUE,k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0),3,3,byrow=TRUE),start,bw1='NRD0',bw2='NRD0',bw3='NRD0',lty=c(1,1,1),lwd=c(1,1,1),col=c(1,1,1),ltythr=1,lwdthr=1,colthr=1,cex.leg=1,...){

plot(density(k1, bw=bw1),xlim=c(mean(k1)-3*sd(k1),mean(k3)+3*sd(k3)),main="",xlab="",lty=lty[1],lwd=lwd[1],col=col[1],...)
lines(density(k2, bw=bw2),lty=lty[2],lwd=lwd[2],col=col[2],...)
lines(density(k3, bw=bw3),lty=lty[3],lwd=lwd[3],col=col[3],...)

Thr0<-thresNLM3(start,k1,k2,k3,rho,costs)

v.eq<-c(Thr0$T1, Thr0$T2);

if(thr==TRUE) { abline(v=v.eq, lty=ltythr, lwd=lwdthr, col=colthr)  }
if(thrval==TRUE) { legend("topleft",paste(c("T1:","T2:"),c(round(v.eq[1],2),round(v.eq[2],2))),lty=ltythr,lwd=lwdthr,col=colthr,bty="n",cex=cex.leg)
 }
}
