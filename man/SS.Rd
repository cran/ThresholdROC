\name{SS}
\alias{SS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample size estimation (two-state setting)
}
\description{
Estimates the sample size and the optimum sample size ratio needed for a given width, costs, disease prevalence and significance level under the assumption of binormality.
}
\usage{
SS(par1.1, par1.2, par2.1, par2.2=NULL, rho, width,
   costs = matrix(c(0, 0, 1, (1-rho)/rho), 2, 2, byrow = TRUE),
   R = NULL, var.equal = FALSE, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par1.1}{
healthy population mean.
}
  \item{par1.2}{
healthy population standard deviation.
}
  \item{par2.1}{
diseased population mean.
}
  \item{par2.2}{
diseased population standard deviation. It can be omitted when assuming equal variances (that is, when \code{var.equal=TRUE}) and in this situation the common variance is assumed to be equal to \code{par1.2}.
}
  \item{rho}{
disease prevalence.
}
  \item{width}{
desired interval width.
}
  \item{costs}{
cost matrix. Costs should be entered as a 2x2 matrix, where the first row corresponds to the true positive and true negative costs and the second row to the false positive and false negative costs. Default cost values are a combination of costs that yields R=1, which is equivalent to the Youden index method (for details about this concept, see \bold{References}). It must be set to \code{NULL} if the user prefers to set \code{R} (see next argument).
}
  \item{R}{
if the cost matrix \code{costs} is not set, R desired (the algorithm will choose a suitable combination of costs that leads to \code{R}). Default, \code{NULL} (which leads to R=1 using the default \code{costs}).
}
  \item{var.equal}{
a logical variable indicating whether to use equal variances. Default, \code{FALSE}.
}
  \item{alpha}{
significance level for the confidence interval. Default, 0.05.
}
}
\value{
an object of class \code{SS} which is a list with eight components:
\item{ss2}{sample size for the diseased group}
\item{ss1}{sample size for the healthy group}
\item{epsilon}{sample size ratio between non-diseased and diseased subjects}
\item{width}{width of the confidence interval provided by the user}
\item{alpha}{significance level provided by the user}
\item{costs}{cost matrix provided by the user}
\item{R}{R term, the product of the non-disease odds and the cost ratio (for further details about this concept, see \bold{References})}
\item{prev}{disease prevalence provided by the user}

}
\references{
Skaltsa K, Jover L, Carrasco JL. (2010). Estimation of the diagnostic threshold accounting for decision costs and sampling uncertainty. Biometrical Journal 52(5):676-697.
}
\examples{
par1.1 <- 0
par1.2 <- 1
par2.1 <- 2
par2.2 <- 1
rho <- 0.3
width <- 0.5
SS(par1.1, par1.2, par2.1, par2.2, rho, width, var.equal = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sample size}
\keyword{threshold}% __ONLY ONE__ keyword per line
