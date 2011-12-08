\name{DensRatio2}
\alias{DensRatio2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
C
}
\description{
Descr
}
\usage{
DensRatio2(p, switch1, switch2, par1.1, par1.2, par2.1, par2.2, rho2, costs)
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
function(p,switch1,switch2,par1.1,par1.2,par2.1,par2.2,rho2,costs) {
c.t.pos<-costs[1,1]; c.t.neg<-costs[1,2]; c.f.pos<-costs[2,1]; c.f.neg<-costs[2,2]
ratio<-(dens(switch2)(p,par2.1,par2.2)/dens(switch1)(p,par1.1,par1.2))-slope(rho2,costs)
return(ratio)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
