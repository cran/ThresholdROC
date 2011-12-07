thresNLM3 <-
function(start,k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0), 3,3,byrow=TRUE)){
th<-nlm(cost3fun,start,k1,k2,k3,rho,costs)$estimate
re<-list(T1=th[1],T2=th[2],PREV=rho,COSTS=costs)

class(re)<-"THRESNLM3" # Asignar clase al objeto

return(re)
}
