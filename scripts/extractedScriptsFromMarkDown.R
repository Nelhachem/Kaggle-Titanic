rm(list=ls())
train <- read.csv(file="./data/raw//train.csv", header=T, sep=",")
test <- read.csv(file="./data/raw//test.csv", header=T, sep=",")
summary(train)
summary(test)
train.processed <- train
test.processed <- test

# filling the missing value in the Age variable
train.processed$Age[is.na(train.processed$Age)] <- median(train.processed$Age, na.rm=T)
test.processed$Age[is.na(test.processed$Age)] <- median(train.processed$Age, na.rm=T)

# filling the missing value in the Fare variable in test data
test.processed$Fare[is.na(test.processed$Fare)] <- median(train.processed$Fare, 
                                                          na.rm=T)

# fix the missing values in the Embarked variable
# this may be a bit tricky. In order to keep the same level order in the variable
# we have to merge the train and test data together first
test.processed$Survived <- NA
combined <- rbind(train.processed, test.processed)
combined$Embarked[combined$Embarked == ""] <- "S"
combined$Embarked <- factor(combined$Embarked)

# split back to train and test data
train.processed <- combined[1:891,]
test.processed <- combined[-c(1:891),]

save(train.processed, file="../data/processed/train.processed.v1.RData")
save(test.processed, file="../data/processed/test.processed.v1.RData")

require(randomForest)
set.seed(101)

fit <- randomForest(as.factor(Survived) ~ . - PassengerId - Name - Ticket - Cabin,
                    data = train.processed, ntree=2000, importance=T)

test.processed$Survived <- NULL
Prediction <- predict(fit, test.processed)
submit <- data.frame(PassengerId = test.processed$PassengerId, Survived = Prediction)
write.csv(submit, file = "../results/res.randomforest.v1.csv", row.names = FALSE)
# we set the value of original empty cells in Age back to NA
train.processed$Age[is.na(train$Age)] <- NA
test.processed$Age[is.na(test$Age)] <- NA

# install.package("rpart")
require(rpart)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data=train.processed[!is.na(train.processed$Age),], method="anova")

train.processed$Age[is.na(train.processed$Age)] <- predict(Agefit, train.processed[is.na(train.processed$Age),])
test.processed$Age[is.na(test.processed$Age)] <- predict(Agefit, test.processed[is.na(test.processed$Age),])

save(train.processed, file="../data/processed/train.processed.v2.RData")
save(test.processed, file="../data/processed/test.processed.v2.RData")


# install.packages("randomForest")
require(randomForest)
set.seed(101)

fit <- randomForest(as.factor(Survived) ~ . - PassengerId - Name - Ticket - Cabin,
data = train.processed, ntree=2000, importance=T)

test.processed$Survived <- NULL
Prediction <- predict(fit, test.processed)
submit <- data.frame(PassengerId = test.processed$PassengerId, Survived = Prediction)
write.csv(submit, file = "../results/res.randomforest.v2.csv", row.names = FALSE)

test.processed$Survived <- NA
combi <- rbind(train.processed, test.processed)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

combi$Survived <- as.factor(combi$Survived)
train.processed <- combi[1:891,]
test.processed <- combi[892:1309,]
save(train.processed, file="../data/processed/train.processed.v3.RData")
save(test.processed, file="../data/processed/test.processed.v3.RData")

fit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize, family = binomial, data = train.processed)

test.processed$Survived <- NULL
Prediction <-ifelse(predict(fit, newdata=test.processed, type="response")>0.5, "1", "0")  
submit <- data.frame(PassengerId = test.processed$PassengerId, Survived = Prediction)
write.csv(submit, file = "../results/res.randomforest.v3.csv", row.names = FALSE)

fit <- glm(Survived ~ Pclass + Sex+ Sex:Age + poly(Age,2) + poly(SibSp,2) + poly(Parch,2) + poly(Fare,2) + Embarked + Title*Sex + I(FamilySize^2), family = binomial, data = train.processed)
summary(fit)
test.processed$Survived <- NULL
Prediction <-ifelse(predict(fit, newdata=test.processed, type="response")>0.5, "1", "0")  
submit <- data.frame(PassengerId = test.processed$PassengerId, Survived = Prediction)
write.csv(submit, file = "../results/res.randomforest.v4.csv", row.names = FALSE)

require(e1071)
svmfit = svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = train.processed, kernel = "linear", cost = 10, scale = FALSE)
test.processed$Survived <- NULL
Prediction <-predict(svmfit, newdata=test.processed)
submit <- data.frame(PassengerId = test.processed$PassengerId, Survived = Prediction)
write.csv(submit, file = "../results/res.randomforest.v5.csv", row.names = FALSE)

require(glmnet)

require(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
set.seed(101)
trainRows <- c(1:891)
combi$Survived <- combi$Survived[1]
results <- lapply(c(1:5), function(i){
  xTotal = model.matrix(Survived ~ Pclass + Sex + Sex:Age + poly(Age, i) + poly(SibSp,i) + poly(Parch,i) + poly(Fare,i) 
                        + Embarked + Title + poly(FamilySize,i), data = combi)

  x = xTotal[trainRows,]
  y = train.processed$Survived
  
  list(model=cv.glmnet(x, y, alpha=0, family="binomial", type.measure="class", parallel=T), testData=xTotal[-trainRows,])
})
stopCluster(cl)
lapply(results, function(x){
  min(x$model[["cvm"]])
})

cv.fit <- results[[4]]$model
testData <- results[[4]]$testData
test.processed$Survived <- NULL
# Prediction <-predict(cv.fit, newx=testData, type="response")
Prediction <-ifelse(predict(cv.fit, newx=testData, type="response")>0.5, "1", "0") 
rownames(Prediction) <- NULL
submit <- data.frame(PassengerId = test.processed$PassengerId)
submit$Survived <- Prediction
write.csv(submit, file = "./results/res.randomforest.v7.csv", row.names = FALSE)
