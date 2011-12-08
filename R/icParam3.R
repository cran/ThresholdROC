icParam3 <-
function(k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0),3,3,byrow=TRUE),a=0.05,start){
cut0<-thresNLM3(start,k1,k2,k3,rho,costs)

cut<-c(cut0$T1,cut0$T2)

se0<-VarThr3(k1,k2,k3,rho,costs,cut)
se<-c(se0$VAR1,se0$VAR2)

ic1<-c(cut[1]+qnorm(a/2)*se[1],cut[1]+qnorm(1-a/2)*se[1])
ic2<-c(cut[2]+qnorm(a/2)*se[2],cut[2]+qnorm(1-a/2)*se[2])

re<-list(LOW1=ic1[1],UP1=ic1[2],LOW2=ic2[1],UP2=ic2[2],CL=1-a,PREV=rho,COSTS=costs)

class(re)<-"ICPARAM3" # Asignar clase al objeto

return(re)
}
