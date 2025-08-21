\name{thresTH3}
\alias{thresTH3}
\title{
Population-based threshold calculation (three-state setting)
}
\description{
This function estimates the theoretical optimum thresholds for the specific distribution parameters, decision costs and prevalences in a three-state setting.
}
\usage{
thresTH3(dist1, dist2, dist3, par1.1, par1.2,
         par2.1, par2.2, par3.1, par3.2, rho,
         costs = matrix(c(0, 1, 1, rho[1]/rho[2], 0, rho[3]/rho[2], 1, 1, 0),
                        3, 3, byrow = TRUE), q1=0.05, q2=0.5, q3=0.95,
         tol = 10^(-8))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist1}{
distribution to be assumed for the first population. See \bold{Details}.
}
  \item{dist2}{
distribution to be assumed for the second population. See \bold{Details}.
}
  \item{dist3}{
distribution to be assumed for the third population. See \bold{Details}.
}
  \item{par1.1}{
first parameter of the first distribution.
}
  \item{par1.2}{
second parameter of the first distribution.
}
  \item{par2.1}{
first parameter of the second distribution.
}
  \item{par2.2}{
second parameter of the second distribution.
}
  \item{par3.1}{
first parameter of the third distribution.
}
  \item{par3.2}{
second parameter of the third distribution.
}
  \item{rho}{
3-dimensional vector of prevalences.
}
  \item{costs}{
cost matrix. Costs should be entered as a 3x3 matrix, where the first row corresponds to the costs associated with the classification of subjects in state 1 (C11, C12 and C13), second row corresponds to the costs associated with the classification of subjects in state 2 (C21, C22 and C23) and the third row corresponds to the costs associated with classification of subjects in state 3 (C31, C32, C33), where Cij is the cost of classifying an individual of class i as class j. Default cost values are a combination of costs that leads to the same thresholds as the Youden index method (see \bold{References} for details).
}
  \item{q1}{
probability of the distribution taking lower values in order to determine a low quantile. Default, 0.05. See \bold{Details}.
  }
  \item{q2}{
probability of the middle distribution in order to determine a medium quantile. Default, 0.5. See \bold{Details}.
  }
  \item{q3}{
probability of the the distribution taking higher values in order to determine a high quantile. Default, 0.95. See \bold{Details}.
  }
  \item{tol}{
tolerance to be used in function \code{uniroot}. Default, 10^(-8).
}
}
\details{
Parameters \code{dist1}, \code{dist2} and \code{dist3} can be chosen between the following 2-parameter distributions: \code{"beta"}, \code{"cauchy"}, \code{"chisq"} (chi-squared), \code{"gamma"}, \code{"lnorm"} (lognormal), \code{"logis"} (logistic), \code{"norm"} (normal) and \code{"weibull"}.

Parameters \code{q1}, \code{q2} and \code{q3} are used to determine two intervals where the \code{uniroot} function should look for the two threshold estimates. Thus, the first threshold is expected to be found between \code{quantile-1(q1)} and \code{quantile-2(q2)} and the second one, between \code{quantile-2(q2)} and \code{quantile-3(q3)}, being \code{quantile-i()} the quantile function for the i-th distribution, i=1,2,3.
}
\value{
An object of class \code{thresTH3}, which is a list with five components:
  \item{thres1 }{first threshold estimate.}
  \item{thres2 }{second threshold estimate.}
  \item{prev }{prevalences provided by the user.}
  \item{costs }{cost matrix provided by the user.}
  \item{method }{method used in the estimation. For an object of class \code{thresTH3} it is always equal to \code{"theoretical"} (meaning that the population-based method has been used).}
}
\references{
Skaltsa K, Jover L, Fuster D, Carrasco JL. (2012). Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine, 31:1098-1109.
}
\note{
It is assumed that \code{dist1} is the distribution with lower values and \code{dist3} is the one taking higher values. If that is not the case, \code{dist1}, \code{dist2} and \code{dist3} (and the corresponding parameters) are re-ordered as needed.
}

\examples{
# example 1
dist <- "norm"
par1.1 <- 0
par1.2 <- 1
par2.1 <- 2
par2.2 <- 1
par3.1 <- 4
par3.2 <- 1
rho <- c(1/3, 1/3, 1/3)

thresTH3(dist, dist, dist,
  par2.1, par2.2, par1.1, par1.2,
  par3.1, par3.2, rho)
  
  
# example 2
dist1 <- "norm"
dist2 <- "lnorm"
dist3 <- "lnorm"
par1.1 <- 0
par1.2 <- 1
par2.1 <- 1
par2.2 <- 0.5
par3.1 <- 2
par3.2 <- 0.5
rho <- rep(1/3, 3)

thresTH3(dist1, dist2, dist3, par1.1, par1.2, par2.1, par2.2, par3.1, par3.2, rho)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{threshold}

