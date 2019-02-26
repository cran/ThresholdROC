\name{plot.thres3}
\alias{plot.thres3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Thresholds and density plot (three-state setting)
}
\description{
This function provides a graph including the three sample densities, the thresholds and their confidence intervals.
}
\usage{
\method{plot}{thres3}(x, bw = c("nrd0", "nrd0", "nrd0"), ci = TRUE,
  which.boot = c("norm", "perc"), col = c(1, 2, 3, 4),
  lty = c(1, 1, 1, 1, 2), lwd = c(1, 1, 1, 1),
  legend = TRUE, leg.pos = "topleft", leg.cex = 1,
  xlim = NULL, ylim = NULL,
  main = paste0("Threshold estimates", ifelse(ci, " and CIs", "")),
  xlab = "", \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{thres3}.
}
  \item{bw}{
vector containing the bandwith for the first sample in the first position, the bandwith for the second sample in the second position and the bandwith for the third sample in the third position (to be passed to \code{density()}). Default, \code{c("nrd0", "nrd0", "nrd0")}.
}
  \item{ci}{
should the confidence intervals be plotted? Default, \code{TRUE}. No confidence intervals will be plotted if \code{x} does not contain one (that is, \code{x$CI} is \code{NULL}).
}
  \item{which.boot}{
in case \code{x} contains confidence intervals calculated by bootstrapping, which one should be printed? The user can choose between \code{"norm"} (based on normal distribution) or \code{"perc"} (based on percentiles). Default, \code{"norm"}. This argument is ignored if parametric confidence intervals were calculated. 
}
  \item{col}{
a 4-dimensional vector containing:

    \code{col[1]}: color for the density of the first sample
    
    \code{col[2]}: color for the density of the second sample
    
    \code{col[3]}: color for the density of the third sample
    
    \code{col[4]}: color for the thresholds and their corresponding confidence intervals
    
    Default, \code{c(1, 2, 3, 4)}. If \code{length(col)} is not 4, \code{col} will be recycled.
}
  \item{lty}{
a 5-dimensional vector containing:

  \code{lty[1]}: line type for the density of the first sample
  
  \code{lty[2]}: line type for the density of the second sample
  
  \code{lty[3]}: line type for the density of the third sample

  \code{lty[4]}: line type for the thresholds
  
  \code{lty[5]}: line type for the confidence intervals
  
  Default, \code{c(1, 1, 1, 1, 2)}. If \code{length(lty)} is not 5, \code{lty} will be recycled.
}
  \item{lwd}{
a 4-dimensional vector containing:

  \code{lwd[1]}: line width for the density of the first sample
  
  \code{lwd[2]}: line width for the density of the second sample

  \code{lwd[3]}: line width for the density of the third sample

  \code{lwd[4]}: line width for the thresholds and their corresponding confidence intervals
  
  Default, \code{c(1, 1, 1, 1)}. If \code{length(lwd)} is not 4, \code{lwd} will be recycled.
}
  \item{legend}{
logical asking if an automatic legend should be added to the graph. Default, \code{TRUE}.
}
  \item{leg.pos}{
position of the legend. Default, \code{"topleft"}. Ignored if \code{legend=FALSE}.
}
  \item{leg.cex}{
number that reescales the size of the legend. Ignored if \code{legend=FALSE}. Default, 1.
}
  \item{xlim}{
2-dimensional vector indicating the lower and upper limits for x-axis. Default value (NULL) sets those limits automatically.
}
  \item{ylim}{
2-dimensional vector indicating the lower and upper limits for y-axis. Default value (NULL) sets those limits automatically.
}
  \item{main, xlab, \dots}{
further arguments to be passed to \code{plot()}.
}
}
\value{
Estimates of the density functions for the three samples and vertical lines representing the thresholds and their confidence limits are drawn.
}
\references{
Skaltsa K, Jover L, Fuster D, Carrasco JL. (2012). Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine, 31:1098-1109.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thres3}}, \code{\link{lines.thres3}}
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
thres <- thres3(k1, k2, k3, rho, dist1="norm", dist2="norm",
                dist3="norm", start=start, ci.method="param") 
plot(thres, leg.pos="topright")

# not assuming trinormality
thres <- thres3(k1, k2, k3, rho, dist1="lnorm", dist2="norm",
                dist3="norm", ci.method="boot") 
plot(thres, leg.pos="topright", which.boot="perc")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{threshold}
\keyword{plot}% __ONLY ONE__ keyword per line
