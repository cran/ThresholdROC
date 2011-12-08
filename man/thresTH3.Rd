\name{thresTH3}
\alias{thresTH3}
\alias{print.THRESTH3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Threshold estimation given distribution parameters (3 state setting)
}
\description{
Estimates the optimum thresholds given the distribution's parameters, decision costs and prevalence values.
}
\usage{
thresTH3(q1, q21, q23, q3, switch1, switch2, switch3, par1.1, par1.2, par2.1, par2.2, par3.1, par3.2, rho, costs = matrix(c(0, 1, 1, rho[1]/rho[2], 0, rho[3]/rho[2], 1, 1, 0), 3, 3, byrow = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q1}{ first quantile for the first distribution }
  \item{q21}{ second quantile for the first distribution }
  \item{q23}{ first quantile for the second distribution }
  \item{q3}{ second quantile for the second distribution } 
  \item{switch1}{ choose lower value distribution }
  \item{switch2}{ choose intermediate value distribution }
  \item{switch3}{ choose higher value distribution }
  \item{par1.1}{ lower values distribution's mean }
  \item{par1.2}{ lower values distribution's standard deviation }
  \item{par2.1}{ intermediate values distribution's mean }
  \item{par2.2}{ intermediate values distribution's standard deviation }
  \item{par3.1}{ higher values distribution's mean }
  \item{par3.2}{ higher values distribution's standard deviation }
  \item{rho}{ prevalence vector }
  \item{costs}{ cost matrix }
}
\details{ This function internally uses the uniroot function which requires two starting points for the root calculation. Four starting points have to be defined for the two thresholds to be estimated in the 3 state setting.

The function estimates the optimum thresholds given the distribution parameters in a theoretical context. For an estimation on a specific sample in a 3 state setting, see thresNLM3 function of the thresholdROC package. 

Costs should be entered as a 3x3 matrix, where the first row corresponds to the costs associated with the classification of subjects in state 1 (C11, C12 and C13), second row corresponds the costs associated with the classification of subjects in state 2 (C21, C22 and C23) and finally, the third row to the costs associated with classification of subjects in state 3. Default cost values are a combination of costs that yields the same threshold as the Youden Index.

Prevalence values for all three states should be entered as a vector of length 3 (rho1, rho2, rho3).
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\note{
%%  ~~further notes~~
}
\references{ 
Skaltsa K et al. (2011) Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine. In press.
}
\author{
Konstantina Skaltsa
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
### define if distributions will be normal (switch=1)
switch1<-1; switch2<-1; switch3<-1; 

### distribution parameters;
par1.1<-0; par1.2<-1; par2.1<-2; par2.2<-1; par3.1<-4; par3.2<-1; 

## context parameters
rho<-c(1/3,1/3,1/3) # prevalence values, costs will take default values

### give some starting values for the uniroot functions
### those values should be percentiles of the first and third distributions
n1<-100; n2<-100; n3<-100; 
q1<-0.05; q21<-0.6; q23<-0.5; q3<-0.99
thresTH3(q1,q21,q23,q3,switch1,switch2,switch3,par1.1,par1.2,par2.1,par2.2,par3.1,par3.2,rho)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Threshold }
\keyword{ Estimation }

