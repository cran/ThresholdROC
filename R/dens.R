dens <-
function(switch){
if(switch==1) fun<-dnorm
if(switch==2) fun<-dlnorm
if(switch==3) fun<-dbeta
if(switch==4) fun<-dgamma
return(fun)
}
