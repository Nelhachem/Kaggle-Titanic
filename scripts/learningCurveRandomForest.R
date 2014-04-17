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
require(randomForest)
num.of.train.samples <- seq(from=50, to=nrow(train), by=5)
train.matrix <- matrix(0,nrow=length(num.of.train.samples), ncol=20)
val.matrix <- matrix(0,nrow=length(num.of.train.samples), ncol=20)
for(i in c(1:length(num.of.train.samples))){
  for(j in c(1:20)){
    selected <- sample(c(1:nrow(train)), num.of.train.samples[i])
    fit <- randomForest(Survived ~ ., data = train[selected,], ntree=2000, importance=T)
    trainRes <- predict(fit)
    train.matrix[i, j] <- mean(trainRes != train[selected,]$Survived)
    valRes <- predict(object=fit, newdata=validation)
    val.matrix[i, j] <- mean(valRes != validation$Survived)
  }
}

errors <- data.frame(sample.size=num.of.train.samples,
                     train=rowMeans(train.matrix),
                     val=rowMeans(val.matrix))

require(ggplot2)
p <- ggplot(data=errors) 
p + geom_line(aes(x=sample.size, y=train,colour="#000099")) + geom_point(aes(x=sample.size, y=val))
