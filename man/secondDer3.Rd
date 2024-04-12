\name{secondDer3}
\alias{secondDer3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Second partial derivative of the cost function (three-state setting)
}
\description{
This function calculates the second partial derivatives of the cost function at a given pair of thresholds.
}
\usage{
secondDer3(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{thres3}.
}
}
\details{
This function evaluates the second partial derivatives of the cost function at the threshold estimates so that the user can assess if these are positive (meaning that the estimation of the thresholds leads to a minimum in the cost function) or close to zero and hence the minimum of the cost function does not exist. A closed formula is given when assuming trinormality, otherwise the second derivatives are evaluated numerically in the threshold estimates using functions from the package \code{numDeriv}.
}
\value{
The value of the second derivative of the cost function at the given thresholds.
}
\references{
Skaltsa K, Jover L, Fuster D, Carrasco JL. (2012). Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine, 31:1098-1109.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thres3}}
}
\examples{
set.seed(1234)
n <- 100
k1 <- rlnorm(n)
k2 <- rnorm(n, 3, 1)
k3 <- rnorm(n, 5, 1)
rho <- c(1/3, 1/3, 1/3)
start <- c(mean(k1), mean(k3))
x <- thres3(k1, k2, k3, rho, dist1="norm", dist2="norm",
            dist3="norm", start=start, ci.method="param") 

secondDer3(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{derivative}
\keyword{threshold}% __ONLY ONE__ keyword per line
