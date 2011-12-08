\name{derMeanNDisEq}
\alias{derMeanNDisEq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
D
}
\description{
Descr
}
\usage{
derMeanNDisEq(k1, k2, rho2, costs)
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
est<-1/2-(varPooled(k1,k2)*log(slope(rho2,costs)))/((mean(k2)-mean(k1))^2)
return(est)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
