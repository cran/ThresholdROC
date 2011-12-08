\name{ablines3}
\alias{ablines3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimum threshold vertical lines added to density plot (3 state setting)
}
\description{
 This function adds vertical lines to density plot representing the optimum thresholds estimated by chosen methods (3 state setting).
}
\usage{
ablines3(param, paramval, boot, bootval, perc, percval, k1, k2, k3, rho, costs = matrix(c(0, 1, 1, rho[1]/rho[2], 0, rho[3]/rho[2], 1, 1, 0), 3, 3, byrow = TRUE), start, B = 500, a = 0.05, lty = c(1, 1, 1), lwd = c(1, 1, 1), col = c(1, 2, 3), cex.leg = 1, ...)
}
\arguments{
  \item{param}{ if TRUE the parametric confidence interval lines are plotted }
  \item{paramval}{ if TRUE the parametric confidence interval values are plotted as legend} 
  \item{boot}{ if TRUE the bootstrap confidence interval lines are plotted }
  \item{bootval}{ if TRUE the bootstrap confidence interval values are plotted as legend}
  \item{perc}{ if TRUE the percentile confidence interval lines are plotted }
  \item{percval}{ if TRUE the percentile confidence interval values are plotted as legend}
  \item{k1}{ lower values sample vector }
  \item{k2}{ intermediate values sample vector }
  \item{k3}{ higher values sample vector }
  \item{rho}{ prevalence vector }
  \item{costs}{ cost matrix }
  \item{start}{ starting value for the optimisation algorithm}
  \item{B}{ number of resamples generated for the bootstrap methods }
  \item{a}{ 1-confidence level }
  \item{lty}{ linetype for the confidence interval lines }
  \item{lwd}{ linewidth for the confidence interval lines }
  \item{col}{ color for the confidence interval lines }
  \item{cex.leg}{ legend size }
 \item{\dots}{ additional options for line appearance }
}

\details{
The optimum threshold is estimated by minimising the cost function. Estimation methods are described in the references.

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
\references{ 
Skaltsa K et al. (2011) Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine. In press.
}
\author{ Konstantina Skaltsa }

\note{ This function uses the abline function and further arguments can be set to customise the resulting plot }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## distributions' parameters;
par1.1<-0; par1.2<-1; par2.1<-2; par2.2<-1; par3.1<-4; par3.2<-1; 

## context parameters
rho<-c(1/3,1/3,1/3) # prevalence values, costs will take default values

## sample sizes
n1<-100; n2<-100; n3<-100; 

### generate 3 samples
k1<-rnorm(n1,par1.1,par1.2); k2<-rnorm(n2,par2.1,par2.2);  k3<-rnorm(n3,par3.1,par3.2);

start<-c(mean(k1),mean(k3))

dev.new(width=8.5, height=5.5)
plotDens3(thr=TRUE,thrval=TRUE,k1,k2,k3,rho,start=start,bw1='NRD0',bw2='NRD0',bw3='NRD0',lty=c(1,1,1),lwd=c(1,1,1),col=c(1,1,1),ltythr=2,lwdthr=0.5,colthr=4,cex=0.8,ylim=c(0,0.6))
title(main="Marker densities",xlab="Measurements", cex.main=1.6, cex.lab=1.4)
ablines3(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,k1,k2,k3,rho,start=start,B=100,a=0.05,col=c(2,3,1),lty=c(1,1,1),lwd=c(1,1,1),cex=0.8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plot }
\keyword{ Ablines }
\keyword{ Estimation }
