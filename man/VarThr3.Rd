\name{VarThr3}
\alias{VarThr3}
\alias{print.VARMAK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Var
}
\description{
Descr
}
\usage{
VarThr3(k1, k2, k3, rho, costs = matrix(c(0, 1, 1, rho[1]/rho[2], 0, rho[3]/rho[2], 1, 1, 0), 3, 3, byrow = TRUE), Thr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k1}{
%%     ~~Describe \code{k1} here~~
}
  \item{k2}{
%%     ~~Describe \code{k2} here~~
}
  \item{k3}{
%%     ~~Describe \code{k3} here~~
}
  \item{rho}{
%%     ~~Describe \code{rho} here~~
}
  \item{costs}{
%%     ~~Describe \code{costs} here~~
}
  \item{Thr}{
%%     ~~Describe \code{Thr} here~~
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
function(k1,k2,k3,rho,costs=matrix(c(0,1,1, rho[1]/rho[2],0,rho[3]/rho[2], 1,1,0), 3,3,byrow=TRUE),Thr){
c11<-costs[1,1]; c12<-costs[1,2]; c13<-costs[1,3]; c21<-costs[2,1]; c22<-costs[2,2]; c23<-costs[2,3]; c31<-costs[3,1]; c32<-costs[3,2]; c33<-costs[3,3]; 
 rho1<-rho[1]; rho2<-rho[2]; rho3<-rho[3]
sk11<-VDel3st(k1,k2,k3,rho,costs,Thr)[1]
sk12<-A2der3st(k1,k2,k3,rho,costs,Thr)[1]
sk21<-VDel3st(k1,k2,k3,rho,costs,Thr)[2]
sk22<-A2der3st(k1,k2,k3,rho,costs,Thr)[2]
  ret1<-sk11/sk12^2
  ret2<-sk21/sk22^2

re<-list(VAR1=ret1,VAR2=ret2)

class(re)<-"VARMAK" # Asignar clase al objeto

return(re)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
