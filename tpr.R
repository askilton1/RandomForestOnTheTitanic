truePositiveRate <- function(predictions,obs,thresh=0.5){
  funList<-list()
  predictions <- as.numeric(as.character(predictions))
  obs <- as.numeric(as.character(obs))
  funList$cm<-table(ifelse(predictions>thresh,1,0),obs)
  funList$tpr<-funList$cm[2,2]/sum(funList$cm[,2])
  return(funList)
}
