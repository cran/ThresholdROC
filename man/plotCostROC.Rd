\name{plotCostROC}
\alias{plotCostROC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot ROC Curve and Cost Function (2 state setting)
}
\description{
Plots the empirical ROC Curve and the Cost Function associated to the prevalence and costs
}
\usage{
plotCostROC(k1, k2, rho2, costs = matrix(c(0, 0, 1, (1 - rho2)/rho2), 2, 2, byrow = TRUE), plotThr = TRUE, plotAccur = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k1}{ non-diseased sample vector }
  \item{k2}{ diseased sample vector }
  \item{rho2}{ disease prevalence }
  \item{costs}{ cost matrix }
  \item{plotThr}{ if TRUE the estimated threshold line is plotted }
  \item{plotAccur}{ if TRUE the estimated threshold value is plotted as legend}
  \item{\dots}{ additional options for plot appearance }
}
\details{
The output is a two-panel plot; the one on the left shows the cost function associated to the prevalence and user-defined costs and the one on the right, the empirical ROC curve for the data.
If plotThr is TRUE then the threshold value is printed on the cost function plot.

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

\note{	This functions uses the plot function and further arguments can be set to customise the resulting plot }

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

plotCostROC(k1,k2,rho2,plotThr=FALSE,plotAccur=FALSE,fg=1,cex=1.3,cex.lab=1.6,cex.axis=1.4,col=1,col.lab=1,pch="*")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plot }
\keyword{ ROC Curve }