\name{plotCostROC}
\alias{plotCostROC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot cost function and ROC curve
}
\description{
This function plots the ROC curve and the cost function associated to the disease prevalence and costs.
}
\usage{
plotCostROC(x, type = "l", which.plot, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{thres2} or \code{thres3}. See \bold{Details}.
}
  \item{type}{
1-character string giving the type of plot desired. Default, \code{"l"}. See \bold{Details}.
}
  \item{which.plot}{
which plot should be produced? The user can choose between \code{"both"} (default, which plots both cost function and ROC curve), \code{"cost"} (only plots cost function) or \code{"roc"} (only plots ROC curve). It only applies to objects of class \code{thres2}, therefore this argument has no effect if \code{x} is an object of class \code{thres3}.
}
\item{\dots}{
other graphical parameters to be passed to \code{plot()}.
}
}
\details{
If the argument \code{x} was constructed with \code{method="empirical"}, the argument \code{extra.info} should be switched to \code{TRUE} (this only applies when \code{x} is an object of class \code{thres2}).

For parameter \code{type} the following values are possible: \code{"p"} for points, \code{"l"} for lines, \code{"b"} for both points and lines, \code{"c"} for empty points joined by lines, \code{"o"} for overplotted points and lines, \code{"s"} and \code{"S"} for stair steps and \code{"h"} for histogram-like vertical lines. Finally, \code{"n"} does not produce any points or lines.
}
\value{
When \code{x} is an object of class \code{thres2}, two plots are produced. The first one shows the cost function with the cost minimising threshold in red. The second one is the step ROC curve with the sensitivity and specificity achieved in red. If \code{method = "empirical"} is used when building \code{x}, empirical cost function and ROC curve are plotted. If \code{method = "smooth"} is used when building \code{x}, smooth cost function and ROC curve are plotted.

When \code{x} is an object of class \code{thres3}, two plots are produced. The first one shows the cost function C(T1) with the first cost minimising threshold in red. The second one shows the cost function C(T2) with the second cost minimising threshold in red.
}
\references{
Skaltsa K, Jover L, Carrasco JL. (2010). Estimation of the diagnostic threshold accounting for decision costs and sampling uncertainty. Biometrical Journal 52(5):676-697.

Skaltsa K, Jover L, Fuster D, Carrasco JL. (2012). Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine, 31:1098-1109.
}
\note{
This function uses the \code{plot()} function and further arguments can be set to customise the resulting plot.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thres2}}
}
\examples{
\dontrun{
# example 1: x is an object of class 'thres2'
n1 <- 100
n2 <- 100
set.seed(19998)
par1.1 <- 0
par1.2 <- 1
par2.1 <- 2
par2.2 <- 1
rho <- 0.3
k1 <- rnorm(n1, par1.1, par1.2) # non-diseased
k2 <- rnorm(n2, par2.1, par2.2) # diseased
x <- thres2(k1, k2, rho, method = "emp", ci.method = "boot",
            extra = TRUE)

par(mfrow=c(1, 2))
plotCostROC(x)


# example 2: x is an object of class 'thres3'
set.seed(2015)
n <- 100
k1 <- rlnorm(n)
k2 <- rnorm(n, 3, 1)
k3 <- rnorm(n, 5, 1)
rho <- c(1/3, 1/3, 1/3)
y <- thres3(k1, k2, k3, rho, B = 1000, ci.method = "boot",
            dist1 = "lnorm", dist2 = "norm", dist3 = "norm") 

par(mfrow=c(1, 2))
plotCostROC(y)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ROC}
\keyword{threshold}% __ONLY ONE__ keyword per line
