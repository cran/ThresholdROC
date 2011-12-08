\name{quant}
\alias{quant}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Q
}
\description{
Descr
}
\usage{
quant(switch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{switch}{
%%     ~~Describe \code{switch} here~~
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
function(switch){
if(switch==1) fun<-qnorm
if(switch==2) fun<-qlnorm
if(switch==3) fun<-qbeta
if(switch==4) fun<-qgamma
return(fun)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
