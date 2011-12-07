print.thresEmp2 <-
function(x,...)
{
cat("Results")
cat("\n  Threshold: ", x$THRES)
cat("\n  Sensitivity: ", x$SENS)
cat("\n  Specificity: ", x$SPEC)
cat("\n  Minimum Cost: ", x$COST)
cat("\n")
cat("\nParameters used")
cat("\n  Prevalence:", x$PREV)
cat("\n  Costs (Ctp,Cfp,Ctn,Cfn):", x$COSTS)
cat("\n  Slope:", x$SLOPE)
cat("\n")
}
