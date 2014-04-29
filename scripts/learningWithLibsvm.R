library(e1071)
rm(list=ls())
load("data/processed/train.processed.v3.RData")
load("data/processed/test.processed.v3.RData")

costs <- sapply(seq(from=-5, to=15, by=2), function(x){
  2^x
})
gammas <- sapply(seq(from=-15, to=5, by=2), function(x){
  2^x
})
print(date())
result <- lapply(costs, function(x){
  tmp <- sapply(gammas, function(ga){
    svmfit = svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = train.processed, kernel = "radial", 
                 cost = x, gamma=ga, scale = T, cross = 5)
    svmfit$tot.accuracy
  })
})
print(date())

resMatrix <- matrix(unlist(result), ncol=length(costs), byrow=T)
best.pair.ind<-which(resMatrix == max(resMatrix), arr.ind=T)
best.pair <- c(costs[best.pair.ind[1]], gammas[best.pair.ind[2]])

svmfit = svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = train.processed, kernel = "radial", 
             cost = best.pair[1], gamma=best.pair[2], scale = T)
test.processed$Survived <- NULL
Prediction <-predict(svmfit, newdata=test.processed)
submit <- data.frame(PassengerId = test.processed$PassengerId, Survived = Prediction)
write.csv(submit, file = "results/res.svm.v6.csv", row.names = FALSE)


# next time, try a larger range of parameters
# or search around the good pair with smaller steps
# http://stats.stackexchange.com/questions/23803/grid-search-and-tolerance-in-libsvm