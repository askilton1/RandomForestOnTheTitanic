ROC.curve <- function(probs, truth,plot=FALSE){
  pred <- prediction(probs, truth)
  perf <- performance(pred, "tpr", "fpr")
  auc <- unlist(slot(performance(pred,"auc"), "y.values"))
  if(plot==TRUE){
    plot(perf, col="black", lty=3, lwd=3)
    title(paste("AUC = ", round(auc,3)))
    abline(c(0,1))
  }
  return(auc)
}