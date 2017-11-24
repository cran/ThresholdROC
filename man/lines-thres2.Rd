\name{lines.thres2}
\alias{lines.thres2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add threshold lines to a plot (two-state setting)
}
\description{
The function includes vertical lines for the threshold and confidence interval in a plot created with \code{plot.thres2()}.
}

\usage{
\method{lines}{thres2}(x, ci = TRUE, which.boot = c("norm", "perc"),
  col = 1, lty = c(1, 2), lwd = 1, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{thres2}.
}
  \item{ci}{
should the confidence interval be plotted? Default, \code{TRUE}. No confidence interval will be plotted if \code{x} does not contain one (that is, \code{x$CI} is \code{NULL}).
}
  \item{which.boot}{
in case \code{x} contains confidence intervals computed by bootstrapping, which one should be printed? The user can choose between \code{"norm"} (based on normal distribution) or \code{"perc"} (based on percentiles). Default, \code{"norm"}. This argument is ignored if the confidence intervals were computed by the delta method.
}
  \item{col}{
color for the threshold and its corresponding confidence interval. Default, 1.
}
  \item{lty}{
a 2-dimensional vector containing:

  \code{lty[1]}: line type for the threshold
  
  \code{lty[2]}: line type for the confidence interval
  
  Default, \code{c(1, 2)}.
}
  \item{lwd}{
line width for the threshold and its corresponding confidence interval. Default, 1.
}
  \item{\dots}{
further arguments to be passed to \code{abline()}.
}
}
\value{
With a \code{plot.thres2} open, this function adds lines for the required threshold.
}
\references{
Skaltsa K, Jover L, Carrasco JL. (2010). Estimation of the diagnostic threshold accounting for decision costs and sampling uncertainty. Biometrical Journal 52(5):676-697.
}
\seealso{
\code{\link{thres2}}, \code{\link{plot.thres2}}
}

\examples{
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

thres <- thres2(k1, k2, rho, method="eq", ci.method="d")
plot(thres, col=c(1, 2, 4), lwd=c(2, 2, 1), leg.pos="topright")
thresunequal <- thres2(k1, k2, rho, method="unequal", ci=FALSE)
lines(thresunequal, col=3) # almost the same; no confidence interval

\dontrun{
  thresboot <- thres2(k1, k2, rho, method="emp", ci.method="b")
  lines(thresboot, col=5, which.boot="norm")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{threshold}
\keyword{plot}% __ONLY ONE__ keyword per line

