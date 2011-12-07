\name{slope}
\alias{slope}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Slope estimate for the ROC Curve at the optimum threshold (2 state setting)
}
\description{
Estimates the slope value of the ROC Curve at the optimum threshold.
}
\usage{
slope(rho2, costs = matrix(c(0, 0, 1, (1 - rho2)/rho2), 2, 2, byrow = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{rho2}{ disease prevalence }
   \item{costs}{ cost matrix }
}
\details{
The slope value at the optimum threshold depends on the disease prevalence and the user-defined decision costs. More details in the references where the slope is equivalent to the R term found in the cost function.
Costs should be entered as a 2x2 matrix, where the first row corresponds to the true positive and negative costs and the second row to the false positive and negative costs. Default cost values are a combination of costs that yields a slope of 1.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
     Skaltsa K et al. (2010) Estimation of the diagnostic threshold accounting for decision costs
and sampling uncertainty. Biometrical Journal 52(5), 676-697.
}
\author{ Konstantina Skaltsa }
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

rho2<-0.3 # prevalence value

# costs will take the default values that lead to a slope equal to 1
slope(rho2,costs=matrix(c(0,0,1,(1-rho2)/rho2),2,2,byrow=TRUE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ROC Curve }

