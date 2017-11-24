\name{ThresholdROC}
\alias{ThresholdROC}
\alias{ThresholdROC}
\docType{package}
\title{
Optimum threshold estimation based on cost function in a two- and three- state settings
}
\description{
The ThresholdROC package provides point and interval estimations of the optimum threshold as well as graphical tools for continuous diagnostic tests (two- and three- state settings). The point estimation is based on the definition of a cost function which we opt to minimise. An analytical estimator is available for the binormal and trinormal model and the empirical one is used for all settings. The interval estimation is based on the Delta method variance estimator in a binormal parametric setting and on methods on non-linear equations for the trinormal setting. Bootstrap methods are also provided for the confidence intervals.
}
\details{
\tabular{ll}{
Package: \tab ThresholdROC\cr
Type: \tab Package\cr
Version: \tab 2.6\cr
Date: \tab 2017-11-24\cr
License: \tab GPL (>= 2)\cr
}
The most important functions are \code{thres2} and \code{thres3}. They offer a wide range of options for threshold estimation and inference in two and three state settings. We also include the function \code{diagnostic}, which calculates common measures of the accuracy of diagnostic tests involving 2x2 contingency tables of classification results.
}
\author{
Sara Perez-Jaume, Natalia Pallares, Konstantina Skaltsa

Maintainer: Sara Perez-Jaume <spjaume@gmail.com>
}
\references{
Efron B, Tibshirani RJ. (1993). An introduction to the bootstrap, Chapman & Hall.

Perez-Jaume S, Skaltsa K, Pallares N, Carrasco JL. (2017). ThresholdROC: Optimum Threshold Estimation Tools for Continuous Diagnostic Tests in R. Journal of Statistical Software 82(4):1-21. doi: 10.18637/jss.v082.i04.

Skaltsa K, Jover L, Carrasco JL. (2010). Estimation of the diagnostic threshold accounting for decision costs and sampling uncertainty. Biometrical Journal 52(5):676-697.

Skaltsa K, Jover L, Fuster D, Carrasco JL. (2012). Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine, 31:1098-1109.

Zhou XH, Obuchowski NA and McClish DK. (2002). Statistical methods in diagnostic medicine. John Wiley and sons.
}

\keyword{ package }
