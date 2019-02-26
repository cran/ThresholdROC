\name{thres3}
\alias{thres3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Threshold point estimation and confidence intervals (three-state setting)
}
\description{
This function calculates the threshold estimates and their corresponding confidence intervals in a three-state setting.
}
\usage{
thres3(k1, k2, k3, rho,
  costs = matrix(c(0, 1, 1, rho[1]/rho[2], 0, rho[3]/rho[2], 1, 1, 0),
  3, 3, byrow = TRUE), dist1 = "norm", dist2 = "norm",
  dist3 = "norm", start = NULL, ci = TRUE, ci.method = c("param", "boot"),
  B = 1000, alpha = 0.05, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k1}{
vector containing the first sample values.
}
  \item{k2}{
vector containing the second sample values.
}
  \item{k3}{
vector containing the third sample values.
}
  \item{rho}{
3-dimensional vector of prevalences.
}
  \item{costs}{
cost matrix. Costs should be entered as a 3x3 matrix, where the first row corresponds to the costs associated with the classification of subjects in state 1 (C11, C12 and C13), second row corresponds to the costs associated with the classification of subjects in state 2 (C21, C22 and C23) and the third row corresponds to the costs associated with classification of subjects in state 3 (C31, C32, C33), where Cij is the cost of classifying an individual of class i as class j. Default cost values are a combination of costs that leads to the same thresholds as the Youden index method (see \bold{References} for details).
}
  \item{dist1}{
distribution to be assumed for the first population. Default, \code{"norm"}. See \bold{Details}.
}
  \item{dist2}{
distribution to be assumed for the second population. Default, \code{"norm"}. See \bold{Details}.
}
  \item{dist3}{
distribution to be assumed for the third population. Default, \code{"norm"}. See \bold{Details}.
}
  \item{start}{
when the three distributions \code{dist1}, \code{dist2} and \code{dist3} are \code{"norm"}, a 2-dimensional vector containing starting values for the thresholds. The authors recommend to use the mean of the distribution with lower values and the mean of the distribution with higher values. If any distribution is not \code{"norm"}, this parameter is ignored. See \bold{Details}. 
}
  \item{ci}{
  should a confidence interval be calculated? Default, \code{TRUE}. The user can set it to \code{FALSE} to supress the calculation of any confidence interval (in that case, arguments \code{ci.method}, \code{B} and \code{alpha} are ignored).
}
  \item{ci.method}{
method to be used for the confidence intervals calculation. The user can specify just the initial letters. Default, \code{"param"}. See \bold{Details}.
}
  \item{B}{
number of bootstrap resamples when \code{ci.method = "boot"}. Otherwise, ignored. Default, 1000.
}
  \item{alpha}{
significance level for the confidence interval. Default, 0.05.
}
  \item{na.rm}{
a logical value indicating whether \code{NA} values in \code{k1}, \code{k2} and \code{k3} should be stripped before the computation proceeds. Default, \code{FALSE}.
}
}
\details{
Parameters \code{dist1}, \code{dist2} and \code{dist3} can be chosen between the following 2-parameter distributions: \code{"beta"}, \code{"cauchy"}, \code{"chisq"} (chi-squared), \code{"gamma"}, \code{"lnorm"} (lognormal), \code{"logis"} (logistic), \code{"norm"} (normal) and \code{"weibull"}.

For parameter \code{ci.method} the user can choose between \code{"param"} (parametric confidence intervals are calculated when assuming a trinormal underlying model) and \code{"boot"} (the confidence intervals are calculated by bootstrap).

When at least one of the distributions is not \code{"norm"}, the function internally uses the \code{thresTH3()} function, which requires two intervals in which the two thresholds are expected to be found. These intervals are determined by the default values of \code{thresTH3()}. When all the distributions are \code{"norm"}, the function uses the \code{nlm()} function, which requires two starting values (passed through the argument \code{start}).
}

\value{
An object of class \code{thres3}, which is a list with two components:
\item{T}{a list of at least ten components:

  \code{thres1}  first threshold estimate.

  \code{thres2}  second threshold estimate.

  \code{prev}  prevalences provided by the user.
  
  \code{costs}  cost matrix provided by the user.
    
  \code{k1}  vector containing the first sample values provided by the user.
  
  \code{k2}  vector containing the second sample values provided by the user.

  \code{k3}  vector containing the third sample values provided by the user.
  
  \code{dist1}  distribution assumed for the first population.
  
  \code{dist2}  distribution assumed for the second population.
  
  \code{dist3}  distribution assumed for the third population.
  
  When not all distributions are normal, \code{T} also contains:
  
  \code{pars1}  a numeric vector containing the estimation of the parameters of \code{dist1}.
  
  \code{pars2}  a numeric vector containing the estimation of the parameters of \code{dist2}.
  
  \code{pars3}  a numeric vector containing the estimation of the parameters of \code{dist3}.
}

\item{CI }{When \code{ci.method = "param"}, a list of six components:

  \code{lower1}  the lower limit of the confidence interval for the first threshold.

  \code{upper1}  the upper limit of the confidence interval for the first threshold.

  \code{lower2}  the lower limit of the confidence interval for the second threshold.

  \code{upper2}  the upper limit of the confidence interval for the second threshold.

  \code{alpha}  significance level provided by the user.
  
  \code{ci.method} method used for the confidence intervals calculation.
  
  When \code{ci.method = "boot"}, a list of eleven components:
  
  \code{low.norm1}  the lower limit of the bootstrap confidence interval for the first threshold based on the normal distribution.
  
  \code{up.norm1}  the upper limit of the bootstrap confidence interval for the first threshold based on the normal distribution.
  
  \code{low.norm2}  the lower limit of the bootstrap confidence interval for the second threshold based on the normal distribution.
  
  \code{up.norm2}  the upper limit of the bootstrap confidence interval for the second threshold based on the normal distribution.
  
  \code{low.perc1} the lower limit of the bootstrap confidence interval for the first threshold based on percentiles.
  
  \code{up.perc1}  the upper limit of the bootstrap confidence interval for the first threshold based on percentiles.
  
  \code{low.perc2} the lower limit of the bootstrap confidence interval for the second threshold based on percentiles.
  
  \code{up.perc2}  the upper limit of the bootstrap confidence interval for the second threshold based on percentiles.
  
  \code{alpha}  significance level.
  
  \code{B}  number of bootstrap resamples.
  
  \code{ci.method}  method used for the confidence intervals calculation.
  
  When \code{ci = FALSE}, \code{NULL}.
  }

}

\references{
Efron B, Tibshirani RJ. (1993). An introduction to the bootstrap, Chapman & Hall.

Skaltsa K, Jover L, Fuster D, Carrasco JL. (2012). Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine, 31:1098-1109.
}
\note{
It is assumed that \code{k1} is the sample with lower values and \code{k3} is the one taking higher values. If that is not the case, \code{k1}, \code{k2} and \code{k3} (and the corresponding parameters) are re-ordered as needed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thresTH3}}, \code{\link{plot.thres3}}, \code{\link{lines.thres3}}
}
\examples{
set.seed(1234)
n <- 100
k1 <- rlnorm(n)
k2 <- rnorm(n, 3, 1)
k3 <- rnorm(n, 5, 1)
rho <- c(1/3, 1/3, 1/3)

# assuming trinormality
start <- c(mean(k1), mean(k3))
thres3(k1, k2, k3, rho, dist1="norm", dist2="norm", dist3="norm", start=start, ci.method="param") 

# not assuming trinormality
thres3(k1, k2, k3, rho, B=1000, ci.method="boot", dist1="lnorm", dist2="norm", dist3="norm")

# supress confidence intervals calculation
thres3(k1, k2, k3, rho, ci=FALSE, dist1="lnorm", dist2="norm", dist3="norm")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ threshold }
\keyword{ bootstrap }% __ONLY ONE__ keyword per line
