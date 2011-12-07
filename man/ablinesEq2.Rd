\name{ablinesEq2}
\alias{ablinesEq2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Optimum threshold vertical lines added to density plot (equal variances - 2 state setting)
}
\description{
 This function adds vertical lines to density plot representing the optimum thresholds estimated by chosen methods (2 state setting). Equal variances are assumed.
}
\usage{
ablinesEq2(eq.delta, eq.boot, eq.perc, k1, k2, rho2, costs = matrix(c(0, 0, 1, (1 - rho2)/rho2), 2, 2, byrow = TRUE), B = 500, a = 0.05, lty = c(1, 1, 1), lwd = c(1, 1, 1), col = c(1, 2, 3), cex.leg = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{eq.delta}{ if TRUE the delta confidence interval will be plotted }
  \item{eq.boot}{ if TRUE the bootstrap confidence interval will be plotted }
  \item{eq.perc}{ if TRUE the percentile confidence interval will be plotted }
  \item{k1}{ non-diseased sample vector }
  \item{k2}{ diseased sample vector }
  \item{rho2}{ disease prevalence }
  \item{costs}{ cost matrix }
  \item{B}{ number of resamples generated for the bootstrap methods }
  \item{a}{ 1-confidence level }
  \item{lty}{ linetype for the confidence interval lines }
  \item{lwd}{ linewidth for the confidence interval lines }
  \item{col}{ color for the confidence interval lines }
  \item{cex.leg}{ legend size }
  \item{\dots}{ additional options for line appearance }
}
\details{
The optimum threshold is estimated in an equal variance context. For unequal variances see ablinesUn2, or ablinesEmp2 for non-parametric settings. Estimation methods are described in reference 1.

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
 
     2. Efron B Tibshirani RJ An introduction to the bootstrap; Chapman
     & Hall/CRC 1998
}

\author{ Konstantina Skaltsa }

\note{ This function uses the abline function and further arguments can be set to customise the resulting plot. It can only be used with the plotDens2 function which plots the samples' densities. }


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
n1<-100; n2<-100; # sample sizes
par1.1<-0; par1.2<-1  # non-diseased distribution parameters
par2.1<-2; par2.2<-1  # diseased distribution parameters

# generate two normal samples
k1<-rnorm(n1,par1.1,par1.2); k2<-rnorm(n2,par2.1,par2.2)

rho2<-0.3 # prevalence value, costs will take the default values

dev.new(width=8.5, height=5.5)
plotDens2(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,k1,k2,rho2,bw1='NRD0',bw2='NRD0',lty=c(1,2),cex.axis=1.5,ylim=c(0,0.5),cex=1.2)
title(main="Densities",xlab="Measurements", cex.main=1.8, cex.lab=1.6)
ablinesEq2(TRUE,FALSE,FALSE,k1,k2,rho2,B=100,a=0.05,col=c(1,1,1),lty=c(2,1,1),lwd=c(4,1,1),cex=1.2)
}

\keyword{ Ablines }
\keyword{ Plot }
