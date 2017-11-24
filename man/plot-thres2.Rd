\name{plot.thres2}
\alias{plot.thres2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Threshold and density plot (two-state setting)
}
\description{
This function provides a graph including the sample densities (diseased and non-diseased populations), the threshold and its confidence interval.
}
\usage{
\method{plot}{thres2}(x, bw = c("nrd0", "nrd0"), ci = TRUE,
  which.boot = c("norm", "perc"), col = c(1, 2, 1),
  lty = c(1, 1, 1, 2), lwd = c(1, 1, 1),
  main = paste0("Threshold estimate ", ifelse(ci, "and CI ", ""),
  "(method ", x$T$method, ")"), xlab = "", legend = TRUE,
  leg.pos = "topleft", leg.cex = 1, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{thres2}.
}
  \item{bw}{
vector containing the bandwith for the non-diseased sample in the first position and the bandwith for the diseased sample in the second position (to be passed to \code{density()}). Default, \code{c("nrd0", "nrd0")}.
}
  \item{ci}{
should the confidence interval be plotted? Default, \code{TRUE}. No confidence interval will be plotted if \code{x} does not contain one (that is, \code{x$CI} is \code{NULL}).
}
  \item{which.boot}{
in case \code{x} contains confidence intervals computed by bootstrapping, which one should be printed? The user can choose between \code{"norm"} (based on normal distribution) or \code{"perc"} (based on percentiles). Default, \code{"norm"}. This argument is ignored if the confidence intervals were computed by the delta method.
}
  \item{col}{
a 3-dimensional vector containing:

    \code{col[1]}: color for the density of the non-diseased sample
    
    \code{col[2]}: color for the density of the diseased sample
    
    \code{col[3]}: color for the threshold and its corresponding confidence interval
    
    Default, \code{c(1, 2, 1)}.
}
  \item{lty}{
a 4-dimensional vector containing:

  \code{lty[1]}: line type for the density of the non-diseased sample
  
  \code{lty[2]}: line type for the density of the diseased sample

  \code{lty[3]}: line type for the threshold
  
  \code{lty[4]}: line type for the confidence interval
  
  Default, \code{c(1, 1, 1, 2)}.
}
  \item{lwd}{
a 3-dimensional vector containing:

  \code{lwd[1]}: line width for the density of the non-diseased sample
  
  \code{lwd[2]}: line width for the density of the diseased sample
  
  \code{lwd[3]}: line width for the threshold and its corresponding confidence interval
  
  Default, \code{c(1, 1, 1)}.
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
  \item{main, xlab, \dots}{
further arguments to be passed to \code{plot()}.
}
}
\value{
Estimates of the density functions for both samples and vertical lines representing the threshold and its confidence limits are drawn.
}
\references{
Skaltsa K, Jover L, Carrasco JL. (2010). Estimation of the diagnostic threshold accounting for decision costs and sampling uncertainty. Biometrical Journal 52(5):676-697.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thres2}}, \code{\link{lines.thres2}}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{threshold}
\keyword{plot}% __ONLY ONE__ keyword per line
