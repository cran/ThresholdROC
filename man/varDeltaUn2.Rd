\name{varDeltaUn2}
\alias{varDeltaUn2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Var
}
\description{
Descr
}
\usage{
varDeltaUn2(k1, k2, rho2, costs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k1}{
%%     ~~Describe \code{k1} here~~
}
  \item{k2}{
%%     ~~Describe \code{k2} here~~
}
  \item{rho2}{
%%     ~~Describe \code{rho2} here~~
}
  \item{costs}{
%%     ~~Describe \code{costs} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(k1,k2,rho2,costs){
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
if(mean(k1)>mean(k2)){
rho2<-1-rho2
h<-c.t.pos; c.t.pos<-c.t.neg;c.t.neg<-h
d<-c.f.pos; c.f.pos<-c.f.neg;c.f.neg<-d
g<-k1; k1<-k2; k2<-g
  }
n1<-length(k1); n2<-length(k2)
ctrl<-sqroot(k1,k2,rho2,costs) 
if(ctrl<0) est<-NA
else
est<-(derMeanDisUn(k1,k2,rho2,costs))^2*varMeanUn(k2,n2)+(derMeanNDisUn(k1,k2,rho2,costs))^2*varMeanUn(k1,n1)+(derVarDisUn(k1,k2,rho2,costs))^2*varVarUn(k2,n2)+(derVarNDisUn(k1,k2,rho2,costs))^2*varVarUn(k1,n1)
return(est)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
