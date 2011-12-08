derVarNDisUn <-
function(k1,k2,rho2,costs){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
beta<-slope(rho2,costs)
est<-(-mean(k2)*var(k1)+sd(k2)*sqrt(2*log(sd(k2)*beta/sd(k1))*(var(k2)-var(k1))+(mean(k2)-mean(k1))^2)*sd(k1)+mean(k1)*var(k2)) /(var(k2)-var(k1))^2 +(sd(k2)*(-(var(k2)-var(k1))/var(k1)-2*log(sd(k2)*beta/sd(k1)))*sd(k1) /(2*sqrt(2*log(sd(k2)*beta/sd(k1))*(var(k2)-var(k1))+(mean(k2)-mean(k1))^2)) +sd(k2)*sqrt(2*log(sd(k2)*beta/sd(k1))*(var(k2)-var(k1))+(mean(k2)-mean(k1))^2)/(2*sd(k1))-mean(k2)) /(var(k2)-var(k1)) 
return(est)
}
