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
num.of.train.samples <- seq(from=100, to=nrow(train.processed), by=20)

require(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
source("scripts/cv.randomForest.R")

require(randomForest)
print(date())
ptime <- system.time({
  r <- foreach(i=c(1:length(num.of.train.samples)), .export=c("randomForest")) %dopar% {
    res <- lapply(c(1:10), function(x, i){
      selected <- sample(c(1:nrow(train.processed)), num.of.train.samples[i])
      fit <- randomForest(Survived ~ ., data = train.processed[selected,], ntree=500, importance=T)
      trainRes <- predict(fit)
      train.error <- mean(trainRes != train.processed[selected,]$Survived)
      cv.err <- cv.randomForest(train.processed[selected,], 5)
      c(train.error, cv.err)
    }, i)
    temp <- matrix(unlist(res), ncol=2, byrow=T)
    iErr <- apply(temp, 2, mean)
  }
})
stopCluster(cl)
print(date())

r <- matrix(unlist(r), ncol=2, byrow=T)

errors <- data.frame(sample.size=num.of.train.samples, train=r[,1], val=r[,2])

require(ggplot2)
p <- ggplot(data=errors) 
p + geom_line(aes(x=sample.size, y=train,colour="#000099")) + geom_point(aes(x=sample.size, y=val))
