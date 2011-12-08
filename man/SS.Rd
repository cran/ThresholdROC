\name{SS}
\alias{SS}
\alias{print.SS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample Size estimation associated to an optimum sample size ratio and decision costs (2 state setting)
}
\description{
Estimates the sample size needed for a given width, costs, disease prevalence, as well as the sample size ratio.
}
\usage{
SS(par1.1, par1.2, par2.1, par2.2, rho2, costs, width, epsilon, a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par1.1}{ non-diseased mean }
  \item{par1.2}{ non-diseased standard deviation }
  \item{par2.1}{ diseased mean }
  \item{par2.2}{ diseased standard deviation }
  \item{rho2}{ disease prevalence }
  \item{costs}{ cost matrix }
  \item{width}{ desired interval width }
  \item{epsilon}{ desired/estimated sample size ratio }
  \item{a}{ 1-confidence level }
}
\details{
Epsilon is the sample size ratio between non-diseased and diseased subjects. The optimum ratio for a specific context can be found using the SSRatio function. 

Costs should be entered as a 2x2 matrix, where the first row corresponds to the true positive and negative costs and the second row to the false positive and negative costs. Default cost values are a combination of costs that yields a slope of 1.
}
\value{
Function returns an "SS" class object with 5 elements: sample size for diseased and non-diseased, the sample size ratio, width and confidence level (given as arguments to the function). See references for more details on the terminology and examples for the exact output.
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

# sample size ratio (epsilon) and the confidence interval width have to be defined
# epsilon can either be fixed or estimated by the SSRatio function as follows:
eps<-SSRatio(par1.1, par1.2, par2.1, par2.2, rho2)

width<-0.5
SS(par1.1,par1.2,par2.1,par2.2,rho2,width=width,epsilon=eps)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sample size }
\keyword{ Estimation }
