rm(list=ls())
load("data//processed//train.processed.v1.RData")

train.processed$Survived <- as.factor(train.processed$Survived)
temp <- which(names(train.processed) %in% c("PassengerId", "Name","Ticket","Cabin"))
train.processed <- train.processed[,-temp]
# ==================== split the data to train and validation ===========
set.seed(101)
rows <- nrow(train.processed)
val.set <- sample(c(1:rows), floor(0.4*rows))
validation <- train.processed[val.set,]
train <- train.processed[-val.set,]

# ================= build the learning curves =============
num.of.train.samples <- seq(from=50, to=nrow(train), by=5)
train.matrix <- matrix(0,nrow=length(num.of.train.samples), ncol=20)
val.matrix <- matrix(0,nrow=length(num.of.train.samples), ncol=20)
for(i in c(1:length(num.of.train.samples))){
  for(j in c(1:20)){
    selected <- sample(c(1:nrow(train)), num.of.train.samples[i])
    fit <- glm(Survived ~ ., family = binomial, data = train[selected,])
    trainRes <- ifelse(predict(fit, type="response")>0.5, "1", "0")
    train.matrix[i, j] <- mean(trainRes != train[selected,]$Survived)
    valRes <- ifelse(predict(object=fit, newdata=validation, type="response")>0.5,
                     "1", "0")
    val.matrix[i, j] <- mean(valRes != validation$Survived)
  }
}

# ================= dirty test of the model ===============
glm.fit <- glm(Survived ~ ., family = binomial, data = train.processed)
load("data//processed//test.processed.v1.RData")
PassengerId <- test.processed$PassengerId
test.processed$Survived <- as.factor(test.processed$Survived)
temp <- which(names(test.processed) %in% c("PassengerId", "Name","Ticket","Cabin"))
test.processed <- test.processed[,-temp]
testRes <- ifelse(predict(glm.fit, newdata=test.processed, type="response")>0.5,
                 "1", "0")
names(testRes) <- NULL
submit <- data.frame(PassengerId = PassengerId, Survived = testRes)
write.csv(submit, file = "./results/res.GLM.v1.csv", row.names = FALSE)
