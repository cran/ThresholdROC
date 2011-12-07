plotDens2 <-
function(eq,eqval,un,unval,emp,empval,k1,k2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE),bw1='NRD0',bw2='NRD0',lty=c(1,1),lwd=c(1,1),col=c(1,1),cex.leg=1,...){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]

if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
}

plot(density(k1, bw=bw1),xlim=c(mean(k1)-3*sd(k1),mean(k2)+3*sd(k2)),main="",xlab="",lty=lty[1],lwd=lwd[1],col=col[1],...)
lines(density(k2, bw=bw2),lty=lty[2],lwd=lwd[2],col=col[2],...)

if(eq==TRUE) { 
v.eq<-thresEq2(k1,k2,rho2,costs)[[1]]
abline(v=v.eq) 
}
if(eqval==TRUE) { 
legend("topleft",paste("Equal Var:", round(v.eq,3)),lty=1,col=1,bty="n",cex=cex.leg)
}
if(un==TRUE) { 
v.un<-thresUn2(k1,k2,rho2,costs)[[1]]
abline(v=v.un,lty=2) 
}
if(unval==TRUE) { 
legend("topleft",paste("Unequal Var:", round(v.un,3)),lty=2,col=1,bty="n",inset=c(0,0.1),cex=cex.leg)
}
if(emp==TRUE) { 
v.emp<-thresEmp2(k1,k2,rho2,costs)[[1]]
abline(v=v.emp,lty=3) 
}
if(empval==TRUE) { 
legend("topleft",paste("Empirical:", round(v.emp,3)),lty=3,col=1,bty="n",inset=c(0,0.2),cex=cex.leg)
}
}
