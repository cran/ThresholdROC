print.DELTAEQ <-
function(x,...)
{
cat("Results")
cat("\n  Lower Limit: ", x$LOW)
cat("\n  Upper Limit: ", x$UP)
cat("\n")
cat("\nParameters used")
cat("\n  Confidence Level: ", x$CL)
cat("\n  Prevalence:", x$PREV)
cat("\n  Costs (Ctp,Cfp,Ctn,Cfn):", x$COSTS)
cat("\n  Slope:", x$SLOPE)
cat("\n")
}
