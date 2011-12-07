rand <-
function(switch){
if(switch==1) fun<-rnorm
if(switch==2) fun<-rlnorm
if(switch==3) fun<-rbeta
if(switch==4) fun<-rgamma
return(fun)
}
