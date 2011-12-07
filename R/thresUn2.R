thresUn2 <-
function(k1,k2,rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2, byrow=TRUE)){

c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
}
    ctrl<-sqroot(k1,k2,rho2,costs) 
if (ctrl>=0){
cut<-(var(k2)*mean(k1)-var(k1)*mean(k2)+sd(k1)*sd(k2)*sqrt(ctrl))/(var(k2)-var(k1))}
else {cut<-NA}

beta<-((1-rho2)/rho2)*((c.f.pos-c.t.neg)/(c.f.neg-c.t.pos))

re<-list(THRES=cut,PREV=rho2,COSTS=costs,SLOPE=beta)

class(re)<-"thresUn2" # Asignar clase al objeto

return(re)
}
