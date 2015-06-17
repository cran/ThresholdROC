\name{lines.thres3}
\alias{lines.thres3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add threshold lines to a plot (three-state setting)
}
\description{
The function includes vertical lines for the thresholds and confidence intervals in a plot created with \code{plot.thres3()}.
}
\usage{
\method{lines}{thres3}(x, ci = TRUE, which.boot = c("norm", "perc"),
  col = 1, lty = c(1, 2), lwd = 1, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{thres3}.
}
  \item{ci}{
should the confidence intervals be plotted? Default, \code{TRUE}.
}
  \item{which.boot}{
in case \code{x} contains confidence intervals computed by bootstrapping, which one should be printed? the user can choose between \code{"norm"} (based on normal distribution) or \code{"perc"} (based on percentiles). Default, \code{"norm"}.  This argument is ignored if parametric confidence intervals were computed. 
}
  \item{col}{
color for the thresholds and their corresponding confidence intervals. Default, 1.
}
  \item{lty}{
a 2-dimensional vector containing:

  \code{lty[1]}: line type for the thresholds
  
  \code{lty[2]}: line type for the confidence intervals
  
  Default, \code{c(1, 2)}.
}
  \item{lwd}{
line width for the thresholds and their corresponding confidence intervals. Default, 1.
}
  \item{\dots}{
further arguments to be passed to \code{abline()}.
}
}

\value{
With a \code{plot.thres3} open, this function adds lines for the required threshold estimates.
}
\references{
Skaltsa K, Jover L, Fuster D, Carrasco JL. (2012). Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine, 31:1098-1109.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thres3}}, \code{\link{plot.thres3}}
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
thres1 <- thres3(k1, k2, k3, rho, dist1="norm", dist2="norm",
                 dist3="norm", start=start, ci.method="param") 

# not assuming trinormality
start2 <- c(0.05,0.6,0.5,0.95)
set.seed(2014)
thres2 <- thres3(k1, k2, k3, rho, start=start2, B=1000,
                ci.method="boot", dist1="lnorm", dist2="norm",
                dist3="norm") 
plot(thres2, leg.pos="topright", leg.cex=0.8, col=1:4)
lines(thres1, col=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{threshold}
\keyword{plot}% __ONLY ONE__ keyword per line
