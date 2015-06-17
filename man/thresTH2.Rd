\name{thresTH2}
\alias{thresTH2}
\title{
Population-based threshold computation (two-state setting)
}
\description{
This function estimates the theoretical optimum threshold for the specific distribution parameters, decision costs and disease prevalence in a two-state setting.
}
\usage{
thresTH2(dist1, dist2, par1.1, par1.2, par2.1, par2.2, rho,
  costs = matrix(c(0, 0, 1, (1 - rho)/rho), 2, 2, byrow = TRUE), 
  q1 = 0.05, q2 = 0.95, tol = 10^(-8))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist1}{
distribution to be assumed for the healthy population. See \bold{Details}.
}
  \item{dist2}{
distribution to be assumed for the diseased population. See \bold{Details}.
}
  \item{par1.1}{
first parameter of the distribution chosen for the healthy population.
}
  \item{par1.2}{
second parameter of the distribution chosen for the healthy population.
}
  \item{par2.1}{
first parameter of the distribution chosen for the diseased population.
}
  \item{par2.2}{
second parameter of the distribution chosen for the diseased population.
}
  \item{rho}{
disease prevalence.
}
  \item{costs}{
cost matrix. Costs should be entered as a 2x2 matrix, where the first row corresponds to the true positive and true negative costs and the second row to the false positive and false negative costs. Default cost values are a combination of costs that yields R=1, which would be the equivalent to the Youden index method.
}
  \item{q1}{
probability of the left distribution in order to determine a low quantile. Default, 0.05.
}
  \item{q2}{
probability of the right distribution in order to determine a high quantile. Default, 0.95.
}
  \item{tol}{
tolerance to be used in function \code{uniroot}. Default, 10^(-8).
}
}
\details{
Parameters \code{dist1} and \code{dist2} can be chosen between the following 2-parameter distributions: \code{"beta"}, \code{"cauchy"}, \code{"chisq"} (chi-squared), \code{"gamma"}, \code{"lnorm"} (lognormal), \code{"logis"} (logistic), \code{"norm"} (normal) and \code{"weibull"}.
}
\value{
An object of class \code{thresTH2}, which is a list with five components:
  \item{thres }{threshold estimate.}
  \item{prev }{disease prevalence provided by the user.}
  \item{costs }{cost matrix provided by the user.}
  \item{R }{R term, the product of the non-disease odds and the cost ratio (for further details about this concept, see \bold{References}).}
  \item{method }{method used in the estimation. For an object of class \code{thresTH2} it is always equal to \code{"theoretical"} (meaning that the population-based method has been used).}
}
\references{
Skaltsa K, Jover L, Carrasco JL. (2010). Estimation of the diagnostic threshold accounting for decision costs and sampling uncertainty. Biometrical Journal 52(5):676-697.
}
\note{
It is assumed that \code{dist1} is the distribution with lower values. If not, \code{dist1} and \code{dist2} (and the corresponding parameters) are exchanged.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# example 1
dist1 <- "norm"
dist2 <- "norm"
par1.1 <- 0
par1.2 <- 1
par2.1 <- 2
par2.2 <- 1
rho <- 0.1

thresTH2(dist1, dist2, par1.1, par1.2, par2.1, par2.2, rho)


# example 2
dist1 <- "norm"
dist2 <- "lnorm"
par1.1 <- 0
par1.2 <- 1
par2.1 <- 1
par2.2 <- 0.5
rho <- 0.3

thresTH2(dist1, dist2, par1.1, par1.2, par2.1, par2.2, rho)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{threshold}
