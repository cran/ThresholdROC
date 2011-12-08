\name{plotDens3}
\alias{plotDens3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of the sample densities (3 state setting)
}
\description{
Plots the sample densities using a kernel smoothing
}
\usage{
plotDens3(thr = TRUE, thrval = TRUE, k1, k2, k3, rho, costs = matrix(c(0, 1, 1, rho[1]/rho[2], 0, rho[3]/rho[2], 1, 1, 0), 3, 3, byrow = TRUE), start, bw1 = "NRD0", bw2 = "NRD0", bw3 = "NRD0", lty = c(1, 1, 1), lwd = c(1, 1, 1), col = c(1, 1, 1), ltythr = 1, lwdthr = 1, colthr = 1, cex.leg = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thr}{ if TRUE the estimated threshold values are plotted as a vertical line }
  \item{thrval}{ if TRUE the estimated threshold values are plotted as text }
  \item{k1}{ lower values distribution }
  \item{k2}{ intermediate values distribution }
  \item{k3}{ higher values distribution }
  \item{rho}{ prevalence vector }
  \item{costs}{ cost matrix }
  \item{start}{ starting value for the optimisation algorithm}
  \item{bw1}{ bandwidth for the lower value distribution kernel smoothing }
  \item{bw2}{ bandwidth for the intermediate value distribution kernel smoothing }
  \item{bw3}{ bandwidth for the higher value distribution kernel smoothing }
  \item{lty}{ linetype for the confidence interval lines }
  \item{lwd}{ linewidth for the confidence interval lines }
  \item{col}{ color for the confidence interval lines }
  \item{ltythr}{ linetype for the legend lines }
  \item{lwdthr}{ linewidth for the legend lines }
  \item{colthr}{ color for the legend lines }
  \item{cex.leg}{ legend size }
  \item{\dots}{ additional options for plot appearance }
}
\details{ This function internally uses the plot and density function. Any additional plot parameters are allowed after the function's arguments. Bandwidth can be changed. Default is bw='NRD0'.

Costs should be entered as a 3x3 matrix, where the first row corresponds to the costs associated with the classification of subjects in state 1 (C11, C12 and C13), second row corresponds the costs associated with the classification of subjects in state 2 (C21, C22 and C23) and finally, the third row to the costs associated with classification of subjects in state 3. Default cost values are a combination of costs that yields the same threshold as the Youden Index.

Prevalence values for all three states should be entered as a vector of length 3 (rho1, rho2, rho3).
}
\value{
A density plot showing the three sample densities is shown.
}
\references{ 
Skaltsa K et al. (2011) Optimum threshold estimation based on cost function in a multistate diagnostic setting. Statistics in Medicine. In press.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plot }
\keyword{ Estimation }
