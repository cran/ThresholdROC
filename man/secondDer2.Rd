\name{secondDer2}
\alias{secondDer2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Second partial derivative of the cost function (two-state setting)
}
\description{
This function calculates the second partial derivative of the cost function at a given threshold.
}
\usage{
secondDer2(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{thres2}.
}
}
\details{
This function evaluates the second derivative of the cost function at the threshold estimate so that the user can assess if this is positive (meaning that the estimation of the threshold leads to a minimum in the cost function) or close to zero and hence the minimum of the cost function does not exist. A closed formula is given when assuming binormality, otherwise the second derivative is evaluated numerically in the threshold estimate using functions from the package \code{numDeriv}.
}
\value{
The value of the second derivative of the cost function at the given threshold.
}
\references{
Skaltsa K, Jover L, Carrasco JL. (2010). Estimation of the diagnostic threshold accounting for decision costs and sampling uncertainty. Biometrical Journal 52(5):676-697.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thres2}}
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
x <- thres2(k1, k2, rho, method="equal", ci.method="delta")
secondDer2(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{derivative}
\keyword{threshold}% __ONLY ONE__ keyword per line
