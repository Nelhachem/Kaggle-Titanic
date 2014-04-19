cv.randomForest <- function(data, k = 5){
  folds = sample(rep(1:k, length = nrow(data)))
  cv.errors <- sapply(c(1:k), function(i, data, folds){
    fit <- randomForest(Survived ~ ., data = data[folds != i,], ntree=2000, importance=T)
    pred <- predict(fit, data[folds == i, ])
    error <- mean(pred != data$Survived[folds == i])
  }, data, folds)
  return(mean(cv.errors))
}