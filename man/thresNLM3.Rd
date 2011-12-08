\name{thresNLM3}
\alias{thresNLM3}
\alias{print.THRESNLM3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Threshold estimation for a trinormal setting (3 state setting)
}
\description{
Estimates the optimum thresholds given the distribution's parameters, cost and prevalence values by minimising the cost function (nlm algorithm).
}
\usage{
thresNLM3(start, k1, k2, k3, rho, costs = matrix(c(0, 1, 1, rho[1]/rho[2], 0, rho[3]/rho[2], 1, 1, 0), 3, 3, byrow = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{ vector with two starting values }
  \item{k1}{ lower values sample vector }
  \item{k2}{ intermediate values sample vector }
  \item{k3}{ higher values sample vector }
  \item{rho}{ prevalence vector }
  \item{costs}{ cost matrix }
}
\details{
The function uses the nlm function for the cost function minimisation.

Costs should be entered as a 3x3 matrix, where the first row corresponds to the costs associated with the classification of subjects in state 1 (C11, C12 and C13), second row corresponds the costs associated with the classification of subjects in state 2 (C21, C22 and C23) and finally, the third row to the costs associated with classification of subjects in state 3. Default cost values are a combination of costs that yields the same threshold as the Youden Index.

Prevalence values for all three states should be entered as a vector of length 3 (rho1, rho2, rho3).
}
\value{
Function returns a "thres3" class object with 2 elements: the two thresholds. See references for more details on the terminology and examples for the exact output.
}
\references{ 
Skaltsa K et al. (2011) Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine. In press.
}
\author{
Konstantina Skaltsa
}
\note{
Cost function does not always have a minimum. This depends on the sample data, as well as on the prevalence and decision costs assigned. If the nlm algorithm does not seem to converge, then check the second partial derivatives (SecondDer3st function of this package) for close to zero values.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
### distributions' parameters;
par1.1<-0; par1.2<-1; par2.1<-2; par2.2<-1; par3.1<-4; par3.2<-1; 

## context parameters
rho<-c(1/3,1/3,1/3) # prevalence values, costs will take default values

## sample sizes
n1<-100; n2<-100; n3<-100; 

set.seed(439)
k1<-rnorm(n1,par1.1,par1.2); k2<-rnorm(n2,par2.1,par2.2);  k3<-rnorm(n3,par3.1,par3.2);

## define starting points for the nlm function used for the threshold estimation
start<-c(mean(k1),mean(k3))

thresNLM3(start,k1,k2,k3,rho)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Estimation }
\keyword{ Threshold }
