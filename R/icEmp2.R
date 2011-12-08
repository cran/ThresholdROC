icEmp2 <-
function(k1,k2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE),B=500,a=0.05){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]

if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
}

t<-resample2(k1,k2,B)
t0<-t[[1]]
t1<-t[[2]]

cut<-rep(NA,B)
cut<-sapply(1:B,function(j){
thresEmp2(t0[,j],t1[,j],rho2,costs)[[1]]
})
mean.cut<-mean(cut)
est.se<-sd(cut)

est.cut.emp<-thresEmp2(k1,k2,rho2,costs)[[1]]

###### 1) NORMAL-BOOTSTRAP SE
norm<-c(est.cut.emp+qnorm(a/2)*est.se,est.cut.emp+qnorm(1-a/2)*est.se)

###### 2) PERCENTIL
percentil<-(c(quantile(cut,a/2),quantile(cut,1-a/2)))

beta<-((1-rho2)/rho2)*((c.f.pos-c.t.neg)/(c.f.neg-c.t.pos))

re<-list(LOWBOOT=norm[1], UPBOOT=norm[2], LOWPERC=percentil[1], UPPERC=percentil[2], CL=1-a, BOOT=B, COSTS=costs,SLOPE=beta,PREV=rho2)

class(re)<-"ICEMP2" # Asignar clase al objeto

return(re)
}
