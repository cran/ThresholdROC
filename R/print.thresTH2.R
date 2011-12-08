print.thresTH2 <-
function(x,...)
{
cat("Results \n")
cat("  Threshold:", x$THRES)
cat("\n")
cat("\nParameters used")
cat("\n  Prevalence:", x$PREV)
cat("\n  Costs (Ctp,Cfp,Ctn,Cfn):", x$COSTS)
cat("\n  Slope:", x$SLOPE)
cat("\n")
}
