\name{resample3}
\alias{resample3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Resample
}
\description{
Descr
}
\usage{
resample3(k1, k2, k3, B = 500)
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
  \item{B}{
%%     ~~Describe \code{B} here~~
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
function(k1,k2,k3,B=500){
n1<-length(k1); n2<-length(k2); n3<-length(k3)
t1<-matrix(sample(k1,n1*B,replace=TRUE),nrow=n1)
t2<-matrix(sample(k2,n2*B,replace=TRUE),nrow=n2)
t3<-matrix(sample(k3,n3*B,replace=TRUE),nrow=n3)
t<-list(t1,t2,t3)
return(t)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
