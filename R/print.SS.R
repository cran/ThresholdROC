print.SS <-
function(x,...)
{
cat("Results")
cat("\n  Optimum SS Ratio: ", x$EPS)
cat("\n\nSample size for")
cat("\n  Diseased: ", x$SS2)
cat("\n  Non-diseased: ", x$SS1)
cat("\n")
cat("\nParameters used")
cat("\n  Confidence Level: ", x$CL)
cat("\n  CI width: ", x$WID)
cat("\n  Prevalence:", x$PREV)
cat("\n  Costs (Ctp,Cfp,Ctn,Cfn):", x$COSTS)
cat("\n  Slope:", x$SLOPE)
cat("\n")
}
