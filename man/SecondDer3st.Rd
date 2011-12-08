\name{SecondDer3st}
\alias{SecondDer3st}
\title{ Second partial derivative of the cost function (3 state setting)
}
\description{ 
  Calculates the second partial derivatives of the cost function at a given value.
}
\usage{
SecondDer3st(k1, k2, k3, rho, costs = matrix(c(0, 1, 1, rho[1]/rho[2], 0, rho[3]/rho[2], 1, 1, 0), 3, 3, byrow = TRUE), Thr)
}
\arguments{
  \item{k1}{ lower values distribution }
  \item{k2}{ intermediate values distribution }
  \item{k3}{ higher values distribution }
  \item{rho}{ prevalence vector }
  \item{costs}{ cost matrix }
  \item{Thr}{ value at which the second derivative is evaluated }
}
\details{
This function calculates the second partial derivative of the cost function at specific threshold values. These threshold values can be either defined or estimated by the thresNLM3 function of this package. Second partial derivatives close to zero indicate that the cost function does not have a minimum and the threshold estimate is not reliable. 

Costs should be entered as a 3x3 matrix, where the first row corresponds to the costs associated with the classification of subjects in state 1 (C11, C12 and C13), second row corresponds the costs associated with the classification of subjects in state 2 (C21, C22 and C23) and finally, the third row to the costs associated with classification of subjects in state 3. Default cost values are a combination of costs that yields the same threshold as the Youden Index.

Prevalence values for all three states should be entered as a vector of length 3 (rho1, rho2, rho3).
}
\value{
      \item{comp1 }{second partial derivative of the cost function with respect to threshold 1}
      \item{comp2 }{second partial derivative of the cost function with respect to threshold 2}
}
\references{ 
Skaltsa K et al. (2011) Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine. In press.
}
\author{ Konstantina Skaltsa }


\section{Warning }{If output values are close to zero, then the threshold and its variance estimation will not reliable} 


\examples{
### distributions' parameters;
par1.1<-0; par1.2<-1; par2.1<-2; par2.2<-1; par3.1<-4; par3.2<-1; 

## context parameters
rho<-c(1/3,1/3,1/3) # prevalence values, costs will take default values

## sample sizes
n1<-100; n2<-100; n3<-100; 

set.seed(439)
k1<-rnorm(n1,par1.1,par1.2); k2<-rnorm(n2,par2.1,par2.2);  k3<-rnorm(n3,par3.1,par3.2);

## define starting points for the nlm function used for the threshold estimation
start<-c(mean(k1),mean(k3))

## define threshold values or estimate them with thresNLM3 function
Thr0<-thresNLM3(start,k1,k2,k3,rho)
Thr<-c(Thr0$T1, Thr0$T2); 

# second partial derivative of the cost function should be positive for a minimum to exist
SecondDer3st(k1,k2,k3,rho,Thr=Thr)
}
\keyword{ Derivatives }
