\name{th2st3}
\alias{th2st3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
T
}
\description{
Descr
}
\usage{
th2st3(k1, rho1, n1, n2, n3, c11, c12, Th)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k1}{
%%     ~~Describe \code{k1} here~~
}
  \item{rho1}{
%%     ~~Describe \code{rho1} here~~
}
  \item{n1}{
%%     ~~Describe \code{n1} here~~
}
  \item{n2}{
%%     ~~Describe \code{n2} here~~
}
  \item{n3}{
%%     ~~Describe \code{n3} here~~
}
  \item{c11}{
%%     ~~Describe \code{c11} here~~
}
  \item{c12}{
%%     ~~Describe \code{c12} here~~
}
  \item{Th}{
%%     ~~Describe \code{Th} here~~
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
function(k1,rho1,n1,n2,n3,c11,c12,Th){
  par1.1<-mean(k1); par1.2<-sd(k1)
  der<--(n1+n2+n3)/(sqrt(2*pi)*par1.2^2)*rho1*(c11-c12)*exp(-(Th-par1.1)^2/(2*par1.2^2))*(-1+((Th-par1.1)^2/par1.2^2))
return(der)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
