\name{thres2}
\alias{thres2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Threshold point estimation and confidence intervals (two-state setting)
}
\description{
This function calculates the threshold estimate and its corresponding confidence interval in a two-state setting.
}
\usage{
thres2(k1, k2, rho,
  costs = matrix(c(0, 0, 1, (1 - rho)/rho), 2, 2, byrow = TRUE),
  method = c("equal", "unequal", "empirical", "parametric"),
  dist1 = NULL, dist2 = NULL, ci = TRUE, ci.method = c("delta", "boot"),
  B = 1000, alpha = 0.05, extra.info = FALSE, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k1}{
vector containing the healthy sample values.
}
  \item{k2}{
vector containing the diseased sample values.
}
  \item{rho}{
disease prevalence.
}
  \item{costs}{
cost matrix. Costs should be entered as a 2x2 matrix, where the first row corresponds to the true positive and true negative costs and the second row to the false positive and false negative costs. Default cost values are a combination of costs that yields R=1, which would be the equivalent to the Youden index method (for details about this concept, see \bold{References}).
}
  \item{method}{
method used in the estimation. The user can specify just the initial letters. Default, \code{"equal"}. See \bold{Details} for more information about the methods available.
}
  \item{dist1}{
distribution to be assumed for the healthy population. See \bold{Details}.

}
  \item{dist2}{
distribution to be assumed for the diseased population. See \bold{Details}.

}
  \item{ci}{
  should a confidence interval be calculated? Default, \code{TRUE}. The user can set it to \code{FALSE} to supress the calculation of any confidence interval (in that case, arguments \code{ci.method}, \code{B} and \code{alpha} are ignored).
}
  \item{ci.method}{
method to be used for the confidence intervals calculation. The user can specify just the initial letters. Default, \code{"delta"}. See \bold{Details} for more information about the methods available.
}
  \item{B}{
number of bootstrap resamples when \code{ci.method = "boot"}. Otherwise, ignored. Default, 1000.
}
  \item{alpha}{
significance level for the confidence interval. Default, 0.05.
}
  \item{extra.info}{
when using \code{method="empirical"}, if set to \code{TRUE} the function returns extra information about the calculation of the threshold. Ignored when \code{method} is not \code{"empirical"}. Default, \code{FALSE}.
}
  \item{na.rm}{
a logical value indicating whether \code{NA} values in \code{k1} and \code{k2} should be stripped before the computation proceeds. Default, \code{FALSE}.
}
}
\details{
For parameter \code{method} the user can choose between \code{"equal"} (assumes binormality and equal variances), \code{"unequal"} (assumes binormality and unequal variances), \code{"empirical"} (leaves out any distributional assumption) or \code{"parametric"} (based on the distribution assumed for the two populations).

Parameters \code{dist1} and \code{dist2} can be chosen between the following 2-parameter distributions: \code{"beta"}, \code{"cauchy"}, \code{"chisq"} (chi-squared), \code{"gamma"}, \code{"lnorm"} (lognormal), \code{"logis"} (logistic), \code{"norm"} (normal) and \code{"weibull"}. Notice that \code{dist1} and \code{dist2} are only needed when \code{method = "parametric"}.

For parameter \code{ci.method} the user can choose between \code{"delta"} (delta method is used to estimate the threshold standard error assuming a binormal underlying model) or \code{"boot"} (the confidence interval is calculated by bootstrap).
}
\value{
An object of class \code{thres2}, which is a list with two components:
\item{T}{a list of at least seven components:

  \code{thres}  threshold estimate.
  
  \code{prev}  disease prevalence provided by the user.
  
  \code{costs}  cost matrix provided by the user.
  
  \code{R}  R term, the product of the non-disease odds and the cost ratio (for further details about this concept, see \bold{References}).
  
  \code{method}  method used in the estimation.
  
  \code{k1}  vector containing the healthy sample values provided by the user.
  
  \code{k2}  vector containing the diseased sample values provided by the user.
  
  When \code{method = "empirical"}, \code{T} also contains:
  
  \code{sens}  sensitivity obtained.
  
  \code{spec}  specificity obtained.
  
  \code{cost}  the minimum cost associated with \code{T$thres}.
  
  \code{tot.thres} vector of possible thresholds. Only if \code{extra.info = TRUE}.
  
  \code{tot.cost} vector of empirical costs. Only if \code{extra.info = TRUE}.
  
  \code{tot.spec.c}  complementary of the vector of empirical specificities (1-spec). Only if \code{extra.info = T}.
  
  \code{tot.sens}  vector of empirical sensitivities. Only if \code{extra.info = TRUE}.
  
  When \code{method = "parametric"}, \code{T} also contains:
  
  \code{dist1}  distribution assumed for the healthy population.
  
  \code{dist2}  distribution assumed for the diseased population.
  
  \code{pars1}  a numeric vector containing the estimation of the parameters of \code{dist1}.
  
  \code{pars2}  a numeric vector containing the estimation of the parameters of \code{dist2}.

  }
\item{CI }{When \code{ci.method = "delta"}, a list of five components:

  \code{lower}  the lower limit of the confidence interval.

  \code{upper}  the upper limit of the confidence interval.
  
  \code{se}  the standard error used in the calculation of the confidence interval. 
  
  \code{alpha}  significance level provided by the user.
  
  \code{ci.method} method used for the confidence intervals calculation.
  
  When \code{ci.method = "boot"}, a list of eight components:
  
  \code{low.norm}  the lower limit of the bootstrap confidence interval based on the normal distribution.
  
  \code{up.norm}  the upper limit of the bootstrap confidence interval based on the normal distribution.
  
  \code{se}  the bootstrap standard error used in the calculation of the confidence interval based on the normal distribution. 

  \code{low.perc} the lower limit of the bootstrap confidence interval based on percentiles.
  
  \code{up.perc}  the upper limit of the bootstrap confidence interval based on percentiles.
  
  \code{alpha}  significance level provided by the user.
  
  \code{B}  number of bootstrap resamples used.
  
  \code{ci.method}  method used for the confidence intervals calculation.
  
  When \code{ci = FALSE}, \code{NULL}.
  }
}
\references{
Efron B, Tibshirani RJ. (1993). An introduction to the bootstrap, Chapman & Hall.

Skaltsa K, Jover L, Carrasco JL. (2010). Estimation of the diagnostic threshold accounting for decision costs and sampling uncertainty. Biometrical Journal 52(5):676-697.
}
\note{
It is assumed that \code{k1} is the sample with lower values. If that is not the case, \code{k1} and \code{k2} (and the corresponding parameters) are exchanged.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thresTH2}}, \code{\link{plot.thres2}}, \code{\link{lines.thres2}}
}
\examples{
# example 1
n1 <- 100
n2 <- 100
set.seed(1234)
par1.1 <- 0
par1.2 <- 1
par2.1 <- 2
par2.2 <- 1
rho <- 0.2
k1 <- rnorm(n1, par1.1, par1.2) # non-diseased
k2 <- rnorm(n2, par2.1, par2.2) # diseased

thres2(k1, k2, rho, method="eq", ci.method="d")
thres2(k1, k2, rho, method="uneq", ci.method="d")
\dontrun{
thres2(k1, k2, rho, method="empirical", ci.method="b")

# example 2
set.seed(1234)
k1 <- rnorm(50, 10, 3)
k2 <- rlnorm(55)
rho <- 0.3
thres2(k1, k2, rho, method="param", ci.method="boot", dist1="norm", dist2="lnorm")
}

# supress confidence intervals calculation
thres2(k1, k2, rho, method="equal", ci=FALSE)
thres2(k1, k2, rho, method="empirical", ci=FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ threshold }
\keyword{ bootstrap }% __ONLY ONE__ keyword per line
