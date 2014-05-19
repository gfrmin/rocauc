library(data.table)
library(ggplot2)
theme_set(theme_bw(base_size = 12))

library(ROCR)
rocauc <- function(predictions, actual, label.ordering=NULL, printauc = 0, ...) {
  pred <- prediction(predictions, actual, label.ordering, ...)
  perf <- performance(pred, measure="tpr", x.measure="fpr")
  perfdf <- data.frame(x = perf@x.values, y = perf@y.values, alpha = perf@alpha.values)
  colnames(perfdf) <- c("fpr", "tpr", "cutoff")
  auc <- performance(pred, measure="auc")@y.values[1]
  if (auc < 0.5) {
    return(rocauc(predictions, actual, ..., label.ordering=rev(sort(unique(unlist(actual))))))
  }
  rocplot <- ggplot(perfdf) + geom_line(aes(x = fpr, y = tpr)) + geom_abline(intercept = 0, slope = 1) + annotate(geom="text", x = 0.8, y = 0.0625, size = 15, label = paste0("AUC = ", round(as.numeric(auc), 2))) + labs(x= "FPR", y = "TPR")
  return(rocplot)
}

aucroc <- function(predictions, actual, ...) {
  pred <- prediction(predictions, actual, ...)
  perf <- performance(pred, measure="tpr", x.measure="fpr")
  perfdf <- data.frame(x = perf@x.values, y = perf@y.values, alpha = perf@alpha.values)
  colnames(perfdf) <- c("fpr", "tpr", "cutoff")
  auc <- performance(pred, measure="auc")@y.values[[1]]
  if (auc < 0.5) {
    return(1-auc)
  } else {
    return(auc)
  }
}