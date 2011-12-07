\name{thresTH2}
\alias{thresTH2}
\alias{print.thresTH2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Real optimum threshold for the specific context (2 state setting)
}
\description{
Estimates the theoretical optimum threshold for the specific distribution parameters, decision costs and disease prevalence. 
}
\usage{
thresTH2(q1, q2, switch1, switch2, par1.1, par1.2, par2.1, par2.2, rho2, costs = matrix(c(0, 0, 1, (1 - rho2)/rho2), 2, 2, byrow = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q1}{ quantile of the non-diseased population }
  \item{q2}{ quantile of the diseased population }
  \item{switch1}{ choose a value from 1-4 for the desired non-diseased distribution }
  \item{switch2}{ choose a value from 1-4 for the desired non-diseased distribution }
  \item{par1.1}{ non-diseased mean }
  \item{par1.2}{ non-diseased standard deviation }
  \item{par2.1}{ diseased mean }
  \item{par2.2}{ diseased standard deviation }
  \item{rho2}{ disease prevalence }
  \item{costs}{ cost matrix }
 }
\details{
If distributions and their parameters are known, the real threshold value can be calculated.

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
par1.1 and par1.2 are designated as non-diseased and par2.1 and par2.2 as diseased as in a classical diagnostic textbook. The function though permits a tag exchange in case the non-diseased values are higher than the diseased one. par1.1 and par1.2 are in fact population's parameters with lower values.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

q1<-0.05  # low quantile for the healthy density
q2<-0.95  # high quantile for the diseased density
switch1<-1; switch2<-1  # for normal distributions
par1.1<-0; par1.2<-1  # non-diseased distribution parameters
par2.1<-2; par2.2<-1  # diseased distribution parameters
rho2<-0.3; 

thresTH2(q1,q2,switch1,switch2,par1.1,par1.2,par2.1,par2.2,rho2)  

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Estimation }
\keyword{ Threshold }
