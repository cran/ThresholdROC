\name{SSRatio}
\alias{SSRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample Size Ratio between non-diseased and diseased subjects (2 state setting)
}
\description{
Estimates the optimum ratio between non-diseased and diseased subjects when adequate costs and prevalence are known.
}
\usage{
SSRatio(par1.1, par1.2, par2.1, par2.2, rho2, costs = matrix(c(0, 0, 1, (1 - rho2)/rho2), 2, 2, byrow = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par1.1}{ non-diseased mean }
  \item{par1.2}{ non-diseased standard deviation }
  \item{par2.1}{ diseased mean }
  \item{par2.2}{ diseased standard deviation }
  \item{rho2}{ disease prevalence }
  \item{costs}{ cost matrix }
}
\details{
The output value of this function can be used to estimate the sample sizes by means of the SS function.
} 
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
     Skaltsa K et al. (2010) Estimation of the diagnostic threshold accounting for decision costs
and sampling uncertainty. Biometrical Journal 52(5), 676-697.
}
\author{ Konstantina Skaltsa }

\note{ 
par1.1 and par1.2 are designated as non-diseased and par2.1 and par2.2 as diseased as in a classical diagnostic textbook. The function though permits a tag exchange in case the non-diseased values are higher than the diseased one. par1.1 and par1.2 are in fact population's parameters with lower values.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# define the two distribution parameters (normal)
par1.1<-0; par1.2<-1  # non-diseased distribution parameters
par2.1<-2; par2.2<-1  # diseased distribution parameters

rho2<-0.3 # prevalence value, costs will take the default values

epsilon<-SSRatio(par1.1, par1.2, par2.1, par2.2, rho2)
epsilon
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sample size }
\keyword{ Estimation }
