\name{thresUn2}
\alias{thresUn2}
\alias{print.thresUn2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Threshold estimation assuming unequal variances (2 state setting)
}
\description{
Estimates the optimum threshold assuming binormality and unequal variances.
}
\usage{
thresUn2(k1, k2, rho2, costs = matrix(c(0, 0, 1, (1 - rho2)/rho2), 2, 2, byrow = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k1}{ non-diseased sample vector }
  \item{k2}{ diseased sample vector }
  \item{rho2}{ disease prevalence }
  \item{costs}{ cost matrix }
}
\details{
See references for details on the methods.

Costs should be entered as a 2x2 matrix, where the first row corresponds to the true positive and negative costs and the second row to the false positive and negative costs. Default cost values are a combination of costs that yields a slope of 1.
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
par1.1<-0; par1.2<-1  # non-diseased distribution parameters
par2.1<-2; par2.2<-1  # diseased distribution parameters
rho2<-0.3 # prevalence value, costs will take the default values

# generate two normal samples
n1<-100; n2<-100;
set.seed(19998)
k1<-rnorm(n1,par1.1,par1.2); k2<-rnorm(n2,par2.1,par2.2)

thresUn2(k1, k2, rho2)

# whatever the prevalence values are, costs will compensate and lead to the same slope and optimum threshold
thresUn2(k1, k2, rho2=0.7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Estimation }
\keyword{ Threshold }