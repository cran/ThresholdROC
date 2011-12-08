\name{SecondDer2st}
\alias{SecondDer2st}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Second partial derivative of the cost function (2 state setting)
}
\description{
  Calculates the second partial derivative of the cost function at a given value.
}
\usage{
SecondDer2st(k1, k2, rho2, costs = matrix(c(0, 0, 1, (1 - rho2)/rho2), 2, 2, byrow = TRUE), Thr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k1}{ non-diseased sample vector }
  \item{k2}{ diseased sample vector }
  \item{rho2}{ disease prevalence }
  \item{costs}{ cost matrix }
  \item{Thr}{ value at which the second derivative is evaluated }
}
\details{
This function calculates the second partial derivative of the cost function at specific threshold values. These threshold values can be either defined or estimated by the functions of this package (thresEq2, thresUn2 or thresEmp2). Second partial derivatives close to zero indicate that the cost function does not have a minimum and the threshold estimate is not reliable. 

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
     1. Skaltsa K et al. (2010) Estimation of the diagnostic threshold accounting for decision costs
and sampling uncertainty. Biometrical Journal 52(5), 676-697.
}
\author{ Konstantina Skaltsa }

\note{
k1 is designated as non-diseased and k2 as diseased as in a classical diagnostic textbook. The function though permits a tag exchange in case the non-diseased values are higher than the diseased one. k1 in fact is the sample with lower values.
}

\section{Warning }{If output values are close to zero, then the threshold and its variance estimation will not reliable} 

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
n1<-100; n2<-100;
par1.1<-0; par1.2<-1  # non-diseased distribution parameters
par2.1<-2; par2.2<-1  # diseased distribution parameters

# generate two normal samples
k1<-rnorm(n1,par1.1,par1.2); k2<-rnorm(n2,par2.1,par2.2)

rho2<-0.3;  # prevalence value, costs will take default values

t<-thresEq2(k1, k2, rho2)

SecondDer2st(k1,k2,rho2,Thr=t[[1]])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Derivatives }

