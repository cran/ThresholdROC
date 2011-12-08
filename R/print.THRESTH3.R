print.THRESTH3 <-
function(x,...)
{
cat("Results")
cat("\n  Threshold 1: ", x$T1)
cat("\n  Threshold 2: ", x$T2)
cat("\n")
cat("\nParameters used")
cat("\n  Prevalences:", x$PREV)
cat("\n  Costs")
cat("\n    C11,C21,C31:", x$COSTS[1:3])
cat("\n    C12,C22,C32:", x$COSTS[4:6])
cat("\n    C13,C23,C33:", x$COSTS[7:9])
cat("\n")
}
