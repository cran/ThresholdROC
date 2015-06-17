\name{diagnostic}
\alias{diagnostic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diagnostic tests
}
\description{
This function performs a diagnostic test in a test outcome - condition (presence/absence  of disease) 2x2 table.
}
\usage{
diagnostic(tab, method = c("par", "exact"),
  casecontrol = FALSE, p = NULL, conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
an object of class \code{table} or \code{matrix} in the following form:

    TP  FP
   
    FN  TN
   
where TP=number of true positives, FP=number of false positives, FN=number of false negatives and TN=number of true negatives; that is, a table where the first row corresponds to the true tests and the second row to the false tests; the first column corresponds to the diseased subjects and the second one to the healthy individuals. \code{dim} should be \code{c(2, 2)}.
}
  \item{method}{
method for computing the confidence intervals for sensitivity, specificity, predictive values, accuracy and error rate. The user can choose between \code{"par"} (parametric) and \code{"exact"} (exact). The last one is recommended for small sample sizes. Default, \code{"par"}. The user can specify just the initial letters. See \bold{Details}.
}
  \item{casecontrol}{
were data collected in a case-control study? Default, \code{FALSE}.
}
  \item{p}{
disease prevalence (only when \code{casecontrol=T}).
}
  \item{conf.level}{
confidence level for the confidence intervals. Default, 95\%.
}
}
\details{
For details about the expressions for the statistical measures computed by this function, see \bold{References}.

Since sensitivity, specificity, predictive values, accuracy and error rate are proportions, their confidence intervals are computed using the functions \code{prop.test} (if \code{method = "par"}) and \code{binom.test} (if \code{method = "exact"}) from the \code{stats} package.

Confidence intervals for the likelihood ratios are computed using the formulas proposed in Zhou et al (2002), Section 4.1.3. Furthermore, when likelihood ratios can not be computed due to division by 0, a correction is applied: 0.5 units are added to \code{tab}.

Confidence intervals for the odds ratio are computed using the formulas proposed in Zhou et al (2002), Section 4.1.4. The same correction described before is applied when  the odds ratio can not be computed due to division by 0.

Confidence intervals for the Youden's index are computed using the expression

\deqn{CI(1-\alpha) = (Y-z_{1-\alpha/2}*Var(Y), Y+z_{1-\alpha/2}*Var(Y)),}

where \eqn{Y} is the Youden's index estimate, \eqn{z_{1-\alpha/2}} is the \eqn{1-\alpha/2} quantile of a \eqn{N(0, 1)} distribution and \eqn{Var(Y)} is the variance of the Youden's index estimator, which is computed as \eqn{Var(Sensitivity)+Var(Specificity)}.
}
\value{
A \code{data.frame} with ten rows and three columns containing the point estimate and confidence intervals for the following statistical measures: sensitivity, specificity, positive predictive value, negative predictive value, positive likelihood ratio, negative likelihood ratio, odds ratio, Youden's index, accuracy and error rate. 
}
\references{
Zhou XH, Obuchowski NA and McClish DK. (2002). Statistical methods in diagnostic medicine. John Wiley and sons.

Youden, WJ. (1950). Index for rating diagnostic tests. Cancer 3:32-35.
}
\author{
Sara Perez-Jaume, Natalia Pallares
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thres2}}
}
\examples{
# example 1 (Zhou et al (2002))
japan <- matrix(c(56,23,6,78), ncol=2, byrow=TRUE)
colnames(japan) <- c("D","nD")
rownames(japan) <- c("+","-")
japan
p <- 0.196 # disease prevalence
diagnostic(japan, "par", casecontrol=TRUE, p=p)

# example 2
table <- matrix(c(22,2,3,3), ncol=2, byrow=TRUE)
diagnostic(table, "par")
diagnostic(table, "exact")

# example 3
table2 <- matrix(c(22,2,0,3), ncol=2, byrow=TRUE)
diagnostic(table2, "exact")

# example 4
# generate a random sample of diseased and non-diased subjects
n1 <- 100
n2 <- 100
set.seed(1234)
par1.1 <- 0
par1.2 <- 1
par2.1 <- 2
par2.2 <- 1
k1 <- rnorm(n1, par1.1, par1.2) # non-diseased
k2 <- rnorm(n2, par2.1, par2.2) # diseased
# threshold estimation
rho <- 0.2 # prevalence
thres <- thres2(k1, k2, rho, method="eq", ci.method="d")$T$thres
# diagnostic test using the threshold estimate
marker <- c(k1, k2) # biomarker
condition <- c(rep("nD", length(k1)), rep("D", length(k2))) # condition
test <- ifelse(marker<thres, "-", "+") # test outcome according to thres
# build the table
table3 <- table(test, condition)[2:1, ]
# diagnostic test
round(diagnostic(table3), 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{diagnostic test}