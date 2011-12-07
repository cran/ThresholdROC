thresEmp2 <-
function(k1,k2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE)){

c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
}
n1<-length(k1); n2<-length(k2)
n<-n1+n2
v<-c(k1,k2)
ind.origin<-c(rep(0,n1),rep(1,n2))
vi<-cbind(v,ind.origin)
ord.v<-vi[order(vi[,1],vi[,2]),1:2]
aux<-rep(0,n)
sens<-rep(NA,n)
spec.c<-rep(NA,n)

aux[1]<-1
for(k in 2:n){
if(ord.v[k,1]!=ord.v[k-1,1]){
aux[k]<-1}
else k<-k+1}
for(j in 1:n){
sens[j]<-(sum(ord.v[(j:n),2]))/n2
spec.c[j]<-((n+1-j)-sum(ord.v[(j:n),2]))/n1}

cost.non.par<-c.t.pos*sens*rho2 + c.f.neg*(1-sens)*rho2 + c.f.pos*spec.c*(1-rho2) + c.t.neg*(1-spec.c)*(1-rho2)
total<-cbind(ord.v,aux,cost.non.par,sens,1-spec.c)
ind<-which(aux==1)
tot.omit<-total[ind,]

#ind.min.cost<-which(tot.omit[,4]==min(tot.omit[,4]))
      ind.min.cost <- which(tot.omit[, 4] < min(tot.omit[, 4])+0.001)
sens.s<-tot.omit[ind.min.cost,5]
spec.s<-tot.omit[ind.min.cost,6]
cut.s<-tot.omit[ind.min.cost,1]
cuantos.s<-length(cut.s)
desv.cut.s<-sd(cut.s)

ifelse(length(cut.s)==1,new<-t(tot.omit[ind.min.cost,]),new<-tot.omit[ind.min.cost,])
ind0<-which(new[,2]==0)
ind1<-which(new[,2]==1)

cut.min<-sum( c( rho2*new[ind1,1],(1-rho2)*new[ind0,1] ) )/ ( length(ind0)*(1-rho2)+length(ind1)*rho2)
cost.min<-sum(c(rho2*new[ind1,4],(1-rho2)*new[ind0,4]))/(length(ind0)*(1-rho2)+length(ind1)*rho2)
sens.min<-sum(c(rho2*new[ind1,5],(1-rho2)*new[ind0,5]))/(length(ind0)*(1-rho2)+length(ind1)*rho2)
spec.min<-sum(c(rho2*new[ind1,6],(1-rho2)*new[ind0,6]))/(length(ind0)*(1-rho2)+length(ind1)*rho2)

beta<-((1-rho2)/rho2)*((c.f.pos-c.t.neg)/(c.f.neg-c.t.pos))

re<-list(THRES=cut.min,SENS=sens.min,SPEC=spec.min,COST=cost.min,COSTS=costs,SLOPE=beta,PREV=rho2)

class(re)<-"thresEmp2" # Asignar clase al objeto

return(re)
}
