\name{icDeltaUn2}
\alias{icDeltaUn2}
\alias{print.DELTAUN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Confidence interval for the optimum threshold assuming unequal variances (2 state setting)
}
\description{
Estimates a confidence interval for the optimum threshold assuming unequal variances (2 state setting). The variance is estimated by a delta approximation.
}
\usage{
icDeltaUn2(k1, k2, rho2, costs = matrix(c(0, 0, 1, (1 - rho2)/rho2), 2, 2, byrow = TRUE), a = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k1}{ non-diseased sample vector }
  \item{k2}{ diseased sample vector }
  \item{rho2}{ disease prevalence }
  \item{costs}{ cost matrix }
  \item{a}{ 1-confidence level }
}
\details{
The optimum threshold is estimated in an unequal variance context. For equal variances, see icDeltaEq2. For bootstrap methods on the variance estimation, see icBootEq2 and icBootUn2 for parametric cases and icEmp2 for non-parametric ones. Estimation methods are described in reference 1.

Costs should be entered as a 2x2 matrix, where the first row corresponds to the true positive and negative costs and the second row to the false positive and negative costs. Default cost values are a combination of costs that yields a slope of 1.
}
\value{
Function returns a "DELTAUN" class object with 3 elements: the lower and upper confidence interval limits and the confidence level used.

}
\references{
     1. Skaltsa K et al. (2010) Estimation of the diagnostic threshold accounting for decision costs
and sampling uncertainty. Biometrical Journal 52(5), 676-697.
 
     2. Efron B Tibshirani RJ An introduction to the bootstrap; Chapman
     & Hall/CRC 1998
}
\author{ Konstantina Skaltsa }

\note{ 
k1 is designated as non-diseased and k2 as diseased as in a classical diagnostic textbook. The function though permits a tag exchange in case the non-diseased values are higher than the diseased one. k1 in fact is the sample with lower values.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
n1<-100; n2<-100; # sample sizes
par1.1<-0; par1.2<-1  # non-diseased distribution parameters
par2.1<-2; par2.2<-1  # diseased distribution parameters

# generate two normal samples
k1<-rnorm(n1,par1.1,par1.2); k2<-rnorm(n2,par2.1,par2.2)

rho2<-0.3 # prevalence value, costs will take the default values

icDeltaUn2(k1, k2, rho2) # default confidence level is 0.05
}

\keyword{ Confidence interval }
\keyword{ Estimation }
