quant <-
function(switch){
if(switch==1) fun<-qnorm
if(switch==2) fun<-qlnorm
if(switch==3) fun<-qbeta
if(switch==4) fun<-qgamma
return(fun)
}
