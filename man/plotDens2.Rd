\name{plotDens2}
\alias{plotDens2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of the sample densities (2 state setting)
}
\description{
Plots the sample densities using a kernel smoothing
}
\usage{
plotDens2(eq, eqval, un, unval, emp, empval, k1, k2, rho2, costs = matrix(c(0, 0, 1, (1 - rho2)/rho2), 2, 2, byrow = TRUE), bw1 = "NRD0", bw2 = "NRD0", lty = c(1, 1), lwd = c(1, 1), col = c(1, 1), cex.leg = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eq}{ if TRUE the estimated threshold value assuming equal variances is plotted as a vertical line }
  \item{eqval}{ if TRUE the estimated threshold value assuming equal variances is plotted as text }
  \item{un}{ if TRUE the estimated threshold value assuming unequal variances is plotted as a vertical line }
  \item{unval}{ if TRUE the estimated threshold value assuming unequal variances is plotted as text }
  \item{emp}{ if TRUE the estimated threshold value using the empirical method is plotted as a vertical line }
  \item{empval}{ if TRUE the estimated threshold value using the empirical method is plotted as text }
  \item{k1}{ non-diseased sample vector }
  \item{k2}{ diseased sample vector }
  \item{rho2}{ disease prevalence }
  \item{costs}{ cost matrix }
  \item{bw1}{ bandwidth for the non-diseased kernel smoothing }
  \item{bw2}{ bandwidth for the diseased kernel smoothing }
  \item{lty}{ linetype for the confidence interval lines }
  \item{lwd}{ linewidth for the confidence interval lines }
  \item{col}{ color for the confidence interval lines }
  \item{cex.leg}{ legend size }
  \item{\dots}{ additional options for plot appearance }
}
\details{
This function internally uses the plot and density function. Any additional plot parameters are allowed after the function's arguments.

The output is a two density plot under kernel smoothing and, if requested, a vertical line with an estimated threshold will be plotted. Bandwidth can be modified. Default is bw='NRD0'.

Costs should be entered as a 2x2 matrix, where the first row corresponds to the true positive and negative costs and the second row to the false positive and negative costs. Default cost values are a combination of costs that yields a slope of 1.
}
\value{
A density plot showing the two sample densities is shown.
}
\references{
     Skaltsa K et al. (2010) Estimation of the diagnostic threshold accounting for decision costs
and sampling uncertainty. Biometrical Journal 52(5), 676-697.
 }

\author{ Konstantina Skaltsa }

\note{ This functions uses the plot function and further arguments can be set to customise the resulting plot }


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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plot }
\keyword{ Estimation }
