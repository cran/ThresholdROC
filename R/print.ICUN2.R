print.ICUN2 <-
function(x,...)
{
cat("Results")
cat("\nBootstrap Variance CI")
cat("\n  Lower Limit: ", x$LOWBOOT)
cat("\n  Upper Limit: ", x$UPBOOT)
cat("\nPercentile CI ")
cat("\n  Lower Limit: ", x$LOWPERC)
cat("\n  Upper Limit: ", x$UPPERC)
cat("\n")
cat("\nParameters used")
cat("\n  Confidence Level: ", x$CL)
cat("\n  Prevalence:", x$PREV)
cat("\n  Costs (Ctp,Cfp,Ctn,Cfn):", x$COSTS)
cat("\n  Slope:", x$SLOPE)
cat("\n  Bootstrap Resamples: ", x$BOOT)
cat("\n")
}
