icBoot3 <-
function(k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0),3,3,byrow=TRUE),B=500,a=0.05,start){
t<-resample3(k1,k2,k3,B)
t1<-t[[1]]; t2<-t[[2]]; t3<-t[[3]]

cutsim1<-sapply(1:B,function(k){
thresNLM3(start,t1[,k],t2[,k],t3[,k],rho,costs)$T1
})

cutsim2<-sapply(1:B,function(k){
thresNLM3(start,t1[,k],t2[,k],t3[,k],rho,costs)$T2
})

se1<-sd(cutsim1); se2<-sd(cutsim2)

est<-thresNLM3(start,k1,k2,k3,rho,costs)
est1<-est$T1; est2<-est$T2

###### 1) NORMAL-BOOTSTRAP SE
norm1<-c(est1+qnorm(a/2)*se1,est1+qnorm(1-a/2)*se1)
norm2<-c(est2+qnorm(a/2)*se2,est2+qnorm(1-a/2)*se2)

###### 2) PERCENTIL
perc1<-(c(quantile(cutsim1,a/2),quantile(cutsim1,1-a/2)))
perc2<-(c(quantile(cutsim2,a/2),quantile(cutsim2,1-a/2)))

re<-list(LOWNORM1=norm1[1],UPNORM1=norm1[2],LOWNORM2=norm2[1],UPNORM2=norm2[2],LOWPERC1=perc1[1],UPPERC1=perc1[2],LOWPERC2=perc2[1],UPPERC2=perc2[2],CL=1-a,PREV=rho,COSTS=costs)

class(re)<-"ICBOOT3" # Asignar clase al objeto

return(re)
}
