\name{Der1}
\alias{Der1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
C
}
\description{
Descr
}
\usage{
Der1(p, switch1, switch2, switch3, par1.1, par1.2, par2.1, par2.2, par3.1, par3.2, rho, costs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{switch1}{
%%     ~~Describe \code{switch1} here~~
}
  \item{switch2}{
%%     ~~Describe \code{switch2} here~~
}
  \item{switch3}{
%%     ~~Describe \code{switch3} here~~
}
  \item{par1.1}{
%%     ~~Describe \code{par1.1} here~~
}
  \item{par1.2}{
%%     ~~Describe \code{par1.2} here~~
}
  \item{par2.1}{
%%     ~~Describe \code{par2.1} here~~
}
  \item{par2.2}{
%%     ~~Describe \code{par2.2} here~~
}
  \item{par3.1}{
%%     ~~Describe \code{par3.1} here~~
}
  \item{par3.2}{
%%     ~~Describe \code{par3.2} here~~
}
  \item{rho}{
%%     ~~Describe \code{rho} here~~
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
function(p,switch1,switch2,switch3,par1.1,par1.2,par2.1,par2.2,par3.1,par3.2,rho,costs) {
 rho1<-rho[1]; rho2<-rho[2]; rho3<-rho[3]
c11<-costs[1,1]; c12<-costs[1,2]; c13<-costs[1,3]; c21<-costs[2,1]; c22<-costs[2,2]; c23<-costs[2,3]; c31<-costs[3,1]; c32<-costs[3,2]; c33<-costs[3,3]; 
eq<-(rho1*(c11-c12)*dens(switch1)(p,par1.1,par1.2)+rho2*(c21-c22)*dens(switch2)(p,par2.1,par2.2)+rho3*(c31-c32)*dens(switch3)(p,par3.1,par3.2))
return(eq)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
